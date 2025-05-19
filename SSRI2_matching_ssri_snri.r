requireLibs <- function(libs) {
  suppressWarnings({for(lib in libs){
      if(!require(lib, character.only = T)){
        install.packages(lib)
        require(lib, character.only = T)
      }
    }
  })
}
libs <- c("knitr","doParallel","parallel","data.table","ggfortify","MatchIt","PooledCohort","grf","MatchIt","smd","geepack","Information","missForest","doParallel","foreach","randomForest","itertools","doRNG","arsenal","fastDummies","parallel","stringr","purrr","naniar","dplyr","forestploter")
requireLibs(libs)
rm(requireLibs,libs)

bucket <- Sys.getenv("WORKSPACE_BUCKET")
system(paste("gsutil cp", paste0(bucket,"/data/","data_ssri_snri.csv"), "data_ssri_snri.csv"))
data <- read.csv("data_ssri_snri.csv")


summary(data)

diseases = c("depression","sui","anxiety","bipolar","schizophrenia","adhd")

bucket <- Sys.getenv("WORKSPACE_BUCKET")

for(disease in diseases){
    file_path <- paste0(bucket,"/scores/",disease,"_scores.csv")
    system(paste("gsutil cp", file_path, paste0(disease,"_scores.csv")))
    prs <- read.csv(paste0(disease,'_scores.csv'))
    data <- merge(data,prs,by.x="person_id",by.y="s")
    data <- data %>% rename(!!paste0("prs_",disease) := score)
    }

#save scored sample onto google cloud
write.csv(data, "data_ssri_snri_scored.csv")
system(paste("gsutil cp", "data_ssri_snri_scored.csv", paste0(bucket,"/data/","data_ssri_snri_scored.csv")))

data <- as.data.table(data)
data_filter <- data　# %>% filter(date_drug > "2002-07-01")
data_filter

sample_match <- data_filter

caliper = 0.1 #caliper width for PS matching

W_match = "arm"
X_match = c("gender","age","I(age^2)","rande","edu","income","insurance","birthplace","smoker","sui_pmh","anxiety_pmh","alcohol_pmh","substance_pmh","schizophrenia_pmh","eating_pmh","ptsd_pmh","adhd_pmh","tca_pre","prs_depression","prs_sui","prs_anxiety","prs_bipolar","prs_schizophrenia","prs_adhd") 

createfmla <- function(yvar, xvars) {
    rhs <- paste(xvars, collapse=" + ")
    return(as.formula(paste(yvar, "~", rhs, sep=" ")))
}

#get propensity score
print("Getting propensity score ...")
match_res <- matchit(formula = createfmla(W_match,c(X_match)),
                   data = sample_match,
                   method = "nearest",
                   replace = TRUE,
                   caliper = caliper,
                   std.caliper = FALSE,
                   ratio = 1)
sample_match$PS <- match_res$distance

print("Converting to data table ...")
sample_match <- as.data.table(sample_match)  # Convert to data.table for faster processing

print("Separating treated and control groups ...")
# Separate treated and control groups
sample_treated <- sample_match[get(W_match) == 1]
sample_control <- sample_match[get(W_match) == 0]

sample <- NULL

months_match <- unique(format(as.Date(sample_treated$date_drug,origin="1970-01-01"),format="%Y-%m"))
months_match <- months_match[order(months_match)]

print("Matching ...")
pb2 <- txtProgressBar(min = 1, max = length(months_match), style = 3)

replacement = FALSE

#match within each month of drug initiation, without replacement of matched controls
for(i in 1:length(months_match)){

    setTxtProgressBar(pb2, i)
    subsample_match_treated <- sample_treated[format(as.Date(sample_treated$date_drug,origin="1970-01-01"),format="%Y-%m") == months_match[i]]
    subsample_match_control <- sample_control[format(as.Date(sample_control$date_drug,origin="1970-01-01"),format="%Y-%m") == months_match[i]]

    if(nrow(subsample_match_control) == 0){next} #skip if no control with same drug initiation month exists

    set.seed(1)

    for(j in sample(1:nrow(subsample_match_treated))){
        
        dist = abs(subsample_match_treated$PS[j] - subsample_match_control$PS)

        #no match if greater than caliper width
        if(min(dist) > caliper){next}

        matched_control <- subsample_match_control[which.min(dist)]

        #append to new matched sample
        sample <- rbind(sample, subsample_match_treated[j,], matched_control)
        
        #if replacement==FALSE, remove matched_control from subsample_match_control
        if(!replacement){
            #no replacement
            subsample_match_control <- subsample_match_control[-which.min(dist), ]
            if(nrow(subsample_match_control)==0){break}
        }

    }
}


nrow(sample)
summary(sample)
print(paste("SD of age ",sd(sample$age)))
prop.table(table(sample$gender))
prop.table(table(sample$rande))

sample <- as.data.frame(sample)

nrow(sample)

X_smd = c("gender","age","rande","edu","income","insurance","birthplace","smoker","sui_pmh","anxiety_pmh","alcohol_pmh","substance_pmh","schizophrenia_pmh","eating_pmh","ptsd_pmh","adhd_pmh","tca_pre","prs_depression","prs_sui","prs_anxiety","prs_bipolar","prs_schizophrenia","prs_adhd") 
col_names = c("insurance","birthplace","smoker","sui_pmh","anxiety_pmh","alcohol_pmh","substance_pmh","schizophrenia_pmh","eating_pmh","ptsd_pmh","adhd_pmh","tca_pre")
sample[,col_names] <- lapply(sample[,col_names] , factor)

table1 <- tableby(arm ~ gender + age + rande + edu + income + insurance + birthplace + smoker + sui_pmh + anxiety_pmh + alcohol_pmh + substance_pmh + schizophrenia_pmh + eating_pmh + ptsd_pmh + adhd_pmh + tca_pre + prs_depression + prs_sui + prs_anxiety + prs_bipolar + prs_schizophrenia + prs_adhd,
                  data = sample, total=FALSE, digits=2)

smd <- smd(sample[X_smd], factor(sample$arm))
smd <- smd %>% mutate(SMD = abs(round(estimate, 3))) %>% select(c("variable", "SMD"))
smd_df <- data.frame(
  byvar = "arm",
  variable = X_smd, 
  SMD = smd$SMD
)

table2 <- modpval.tableby(table1, smd_df, use.pname=TRUE)

summary(table2)

table(sample$arm)
table(sample$sui, sample$arm)

# Censor at 5 years
sample$sui <- ifelse(sample$sui_time > 365*5, 0, sample$sui)
sample$sui_time <- ifelse(sample$sui_time > 365*5, 365*5, sample$sui_time)

table(sample$sui, sample$arm)

D = "sui"
Y = "sui_time"
W = "arm"
library(survival)
library(ggfortify)

createint <- function(time, yvar, xvars) {
  int <- paste(xvars, collapse=" * ")
  return(as.formula(paste("Surv(", time, ",",yvar, ") ~", int, sep=" ")))
}

createint_add <- function(time, yvar, xvars, add) {
  int <- paste(xvars, collapse=" * ")
  add <- paste(add, collapse=" + ")
  return(as.formula(paste("Surv(", time, ",",yvar, ") ~", int, " + ", add, sep=" ")))
}


fit <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
               data = sample %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"SNRI"
                                                 )))

autoplot(fit)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample)
summary(cox)


write.csv(x = sample, file = "sample_ssri_snri_scored.csv")

bucket <- Sys.getenv("WORKSPACE_BUCKET")
local_file_path <- file.path("sample_ssri_snri_scored.csv")
remote_file_path <- file.path(bucket, "data", "sample_ssri_snri_scored.csv")
system(paste("gsutil cp", local_file_path, remote_file_path))
