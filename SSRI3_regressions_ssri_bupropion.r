knitr::opts_chunk$set(echo = TRUE)

requireLibs <- function(libs) {
  suppressWarnings({for(lib in libs){
      if(!require(lib, character.only = T)){
        install.packages(lib)
        require(lib, character.only = T)
      }
    }
  })
}
libs <- c("ggpubr","ggfortify","survival","grf","MatchIt","smd","geepack","Information","missForest","doParallel","foreach","randomForest","itertools","doRNG","arsenal","fastDummies","parallel","stringr","purrr","naniar","dplyr","forestploter")
requireLibs(libs)
rm(requireLibs,libs)


bucket <- Sys.getenv("WORKSPACE_BUCKET")
file_path <- paste0(bucket,"/data/sample_ssri_bupropion_scored.csv")
system(paste("gsutil cp", file_path, "sample_ssri_bupropion_scored.csv"))
sample <- read.csv("sample_ssri_bupropion_scored.csv")

D = "sui"
Y = "sui_time"
W = "arm"
X = c("gender","age","rande","edu","income","insurance","birthplace","smoker","sui_pmh","anxiety_pmh","alcohol_pmh","substance_pmh","schizophrenia_pmh","eating_pmh","ptsd_pmh","adhd_pmh","tca_pre","prs_depression","prs_sui","prs_anxiety","prs_bipolar","prs_schizophrenia","prs_adhd") 

createint <- function(time, yvar, xvars) {
  int <- paste(xvars, collapse=" * ")
  return(as.formula(paste("Surv(", time, ",",yvar, ") ~", int, sep=" ")))
}

createint_add <- function(time, yvar, xvars, add) {
  int <- paste(xvars, collapse=" * ")
  add <- paste(add, collapse=" + ")
  return(as.formula(paste("Surv(", time, ",",yvar, ") ~", int, " + ", add, sep=" ")))
}

#full sample
summary(sample)

fit <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
               data = sample %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 ))
               %>% mutate(sui_time = sui_time/7)
              )

autoplot(fit)+
  xlab("Time (weeks)") +
  ylab("Survival Probability") +
  scale_y_continuous(labels = scales::percent, limits = c(0.9,1)) +
  theme_classic() +
  theme(legend.title = element_blank()) + 
  theme(text=element_text(size=16))

cox <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample)
summary(cox)

diseases = c("depression","sui","anxiety","bipolar","schizophrenia","adhd")

for(disease in diseases){
    
    assign(paste0(disease,"_median_score"), median(sample[paste0("prs_",disease)][[1]]))
    sample[paste0("highprs_",disease)] <- factor(ifelse(sample[paste0("prs_",disease)] >= get(paste0(disease,"_median_score")), 1, 0)) 
    
    assign(paste0("fit.", disease), survfit(createint_add(Y, D, paste0("highprs_",disease), "cluster(person_id)"), data=sample))
    print(autoplot(get(paste0("fit.",disease))))
    
    print(disease)
    
    assign(paste0("cox.",disease), coxph(createint_add(Y, D, paste0("highprs_",disease), "cluster(person_id)"), data=sample))
    print(summary(get(paste0("cox.",disease))))
    
}


#gender=="Male"
fit.genderMale <- survfit(createint_add(Y, D, W, "cluster(person_id)"), data=sample[sample$gender=="Male",])
autoplot(fit.genderMale)

cox.genderMale <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$gender=="Male",])
summary(cox.genderMale)

#genderOther
fit.genderOther <- survfit(createint_add(Y, D, W, "cluster(person_id)"), data=sample[sample$gender %in% c("Female","Nonbinary","Other","Transgender"),])
autoplot(fit.genderOther)

cox.genderOther <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$gender %in% c("Female","Nonbinary","Other","Transgender"),])
summary(cox.genderOther)

P.gender <- summary(coxph(createint_add(Y, D, c(W,"gender"), "cluster(person_id)"), data = sample))$coef["arm:genderMale",6]
P.gender

#age>=55
fit.55 <- survfit(createint_add(Y, D, W, "cluster(person_id)"), data=sample[sample$age>=55,])
autoplot(fit.55)

cox.55 <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$age>=55,])
summary(cox.55)

#age 3555
fit.3555 <- survfit(createint_add(Y, D, W, "cluster(person_id)"), data=sample[sample$age<55 & sample$age>=35,])
autoplot(fit.3555)

cox.3555 <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$age<55 & sample$age>=35,])
summary(cox.3555)

#age 35
fit.35 <- survfit(createint_add(Y, D, W, "cluster(person_id)"), data=sample[sample$age<35,])
autoplot(fit.35)

cox.35 <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$age<35,])
summary(cox.35)

P.age.35 <- summary(coxph(createint_add(Y, D, c(W,"I(age < 35)"), "cluster(person_id)"), data = sample))$coef[3,6]
P.age.35

P.age.3555 <- summary(coxph(createint_add(Y, D, c(W,"I(age<55 & age>=35)"), "cluster(person_id)"), data = sample))$coef[3,6]
P.age.3555

P.age.55 <- summary(coxph(createint_add(Y, D, c(W,"I(age>=55)"), "cluster(person_id)"), data = sample))$coef[3,6]
P.age.55

#smoker==1
fit.smoker1 <- survfit(createint_add(Y, D, W, "cluster(person_id)"), data=sample[sample$smoker==1,])
autoplot(fit.smoker1)

cox.smoker1 <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$smoker==1,])
summary(cox.smoker1)

#smoker0
fit.smoker0 <- survfit(createint_add(Y, D, W, "cluster(person_id)"), data=sample[sample$smoker==0,])
autoplot(fit.smoker0)

cox.smoker0 <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$smoker==0,])
summary(cox.smoker0)

P.smoker <- summary(coxph(createint_add(Y, D, c(W,"smoker"), "cluster(person_id)"), data = sample))$coef["arm:smoker",6]
P.smoker

#prs_depression>=depression_median_score
fit.prsdepressionhigh <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                 data=sample[sample$prs_depression>=depression_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prsdepressionhigh)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prsdepressionhigh <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_depression>=depression_median_score,])
summary(cox.prsdepressionhigh)

#prs_depression<depression_median_score
fit.prsdepressionlow <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                data=sample[sample$prs_depression<depression_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prsdepressionlow)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prsdepressionlow <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_depression<depression_median_score,])
summary(cox.prsdepressionlow)

P.prsdepression <- summary(coxph(createint_add(Y, D, c(W,"I(prs_depression<depression_median_score)"), "cluster(person_id)"), data = sample))$coef[3,6]
P.prsdepression

#prs_sui>=sui_median_score
fit.prssuihigh <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                 data=sample[sample$prs_sui>=sui_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prssuihigh)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prssuihigh <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_sui>=sui_median_score,])
summary(cox.prssuihigh)

#prs_sui<sui_median_score
fit.prssuilow <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                data=sample[sample$prs_sui<sui_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prssuilow)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prssuilow <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_sui<sui_median_score,])
summary(cox.prssuilow)

P.prssui <- summary(coxph(createint_add(Y, D, c(W,"I(prs_sui<sui_median_score)"), "cluster(person_id)"), data = sample))$coef[3,6]
P.prssui

#prs_anxiety>=anxiety_median_score
fit.prsanxietyhigh <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                 data=sample[sample$prs_anxiety>=anxiety_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prsanxietyhigh)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prsanxietyhigh <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_anxiety>=anxiety_median_score,])
summary(cox.prsanxietyhigh)

#prs_anxiety<anxiety_median_score
fit.prsanxietylow <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                data=sample[sample$prs_anxiety<anxiety_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prsanxietylow)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prsanxietylow <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_anxiety<anxiety_median_score,])
summary(cox.prsanxietylow)

P.prsanxiety <- summary(coxph(createint_add(Y, D, c(W,"I(prs_anxiety<anxiety_median_score)"), "cluster(person_id)"), data = sample))$coef[3,6]
P.prsanxiety

#prs_bipolar>=bipolar_median_score
fit.prsbipolarhigh <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                 data=sample[sample$prs_bipolar>=bipolar_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prsbipolarhigh)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prsbipolarhigh <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_bipolar>=bipolar_median_score,])
summary(cox.prsbipolarhigh)

#prs_bipolar<bipolar_median_score
fit.prsbipolarlow <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                data=sample[sample$prs_bipolar<bipolar_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prsbipolarlow)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prsbipolarlow <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_bipolar<bipolar_median_score,])
summary(cox.prsbipolarlow)

P.prsbipolar <- summary(coxph(createint_add(Y, D, c(W,"I(prs_bipolar<bipolar_median_score)"), "cluster(person_id)"), data = sample))$coef[3,6]
P.prsbipolar

#prs_schizophrenia>=schizophrenia_median_score
fit.prsschizophreniahigh <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                 data=sample[sample$prs_schizophrenia>=schizophrenia_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prsschizophreniahigh)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prsschizophreniahigh <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_schizophrenia>=schizophrenia_median_score,])
summary(cox.prsschizophreniahigh)

#prs_schizophrenia<schizophrenia_median_score
fit.prsschizophrenialow <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                data=sample[sample$prs_schizophrenia<schizophrenia_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prsschizophrenialow)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prsschizophrenialow <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_schizophrenia<schizophrenia_median_score,])
summary(cox.prsschizophrenialow)

P.prsschizophrenia <- summary(coxph(createint_add(Y, D, c(W,"I(prs_schizophrenia<schizophrenia_median_score)"), "cluster(person_id)"), data = sample))$coef[3,6]
P.prsschizophrenia

#prs_adhd>=adhd_median_score
fit.prsadhdhigh <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                 data=sample[sample$prs_adhd>=adhd_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prsadhdhigh)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prsadhdhigh <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_adhd>=adhd_median_score,])
summary(cox.prsadhdhigh)

#prs_adhd<adhd_median_score
fit.prsadhdlow <- survfit(createint_add(Y, D, W, "cluster(person_id)"), 
                                data=sample[sample$prs_adhd<adhd_median_score,] %>% mutate(arm=
                                        case_when(arm==1~"SSRI", 
                                                  arm==0~"Bupropion"
                                                 )))

autoplot(fit.prsadhdlow)+
  xlab("Time (days)") +
  ylab("Survival Probability") +
  theme_classic() +
  theme(legend.title = element_blank())

cox.prsadhdlow <- coxph(createint_add(Y, D, W, "cluster(person_id)"), data = sample[sample$prs_adhd<adhd_median_score,])
summary(cox.prsadhdlow)

P.prsadhd <- summary(coxph(createint_add(Y, D, c(W,"I(prs_adhd<adhd_median_score)"), "cluster(person_id)"), data = sample))$coef[3,6]
P.prsadhd

round2 <- function(x, digits = 2) {
  formatted_numbers <- formatC(round(x, digits), format = "f", digits = digits)
  return(formatted_numbers)
}

forest.res <- NULL
forest.res$Subgroup <- c("All patients",
                         "Gender",
                         "   Male",
                         "   Female & other",
                         "Age",
                         "   <35",
                         "   35-55",
                         "   ≥55",
                         "Smoker",
                         "   No",
                         "   Yes",
                         "PRS depression",
                         "   Low",
                         "   High",
                         "PRS self-injurious behavior",
                         "   Low",
                         "   High",
                         "PRS anxiety",
                         "   Low",
                         "   High",
                         "PRS bipolar disorder",
                         "   Low",
                         "   High",
                         "PRS schizophrenia",
                         "   Low",
                         "   High",
                         "PRS ADHD",
                         "   Low",
                         "   High"
                        )
forest.res$est <- c(summary(cox)$conf.int[1],
                    NA,
                    summary(cox.genderMale)$conf.int[1],
                    summary(cox.genderOther)$conf.int[1],
                    NA,
                    summary(cox.35)$conf.int[1],
                    summary(cox.3555)$conf.int[1],
                    summary(cox.55)$conf.int[1],
                    NA,
                    summary(cox.smoker0)$conf.int[1],
                    summary(cox.smoker1)$conf.int[1],
                    NA,
                    summary(cox.prsdepressionlow)$conf.int[1],
                    summary(cox.prsdepressionhigh)$conf.int[1],
                    NA,
                    summary(cox.prssuilow)$conf.int[1],
                    summary(cox.prssuihigh)$conf.int[1],
                    NA,
                    summary(cox.prsanxietylow)$conf.int[1],
                    summary(cox.prsanxietyhigh)$conf.int[1],
                    NA,
                    summary(cox.prsbipolarlow)$conf.int[1],
                    summary(cox.prsbipolarhigh)$conf.int[1],
                    NA,
                    summary(cox.prsschizophrenialow)$conf.int[1],
                    summary(cox.prsschizophreniahigh)$conf.int[1],
                    NA,
                    summary(cox.prsadhdlow)$conf.int[1],
                    summary(cox.prsadhdhigh)$conf.int[1]
                   )
forest.res$lcl <- c(summary(cox)$conf.int[c(3)],
                   NA,
                   summary(cox.genderMale)$conf.int[c(3)],
                   summary(cox.genderOther)$conf.int[c(3)],
                   NA,
                   summary(cox.35)$conf.int[c(3)],
                   summary(cox.3555)$conf.int[c(3)],
                   summary(cox.55)$conf.int[c(3)],
                   NA,
                   summary(cox.smoker0)$conf.int[c(3)],
                   summary(cox.smoker1)$conf.int[c(3)],
                   NA,
                   summary(cox.prsdepressionlow)$conf.int[c(3)],
                   summary(cox.prsdepressionhigh)$conf.int[c(3)],
                   NA,
                   summary(cox.prssuilow)$conf.int[c(3)],
                   summary(cox.prssuihigh)$conf.int[c(3)],
                   NA,
                   summary(cox.prsanxietylow)$conf.int[c(3)],
                   summary(cox.prsanxietyhigh)$conf.int[c(3)],
                   NA,
                   summary(cox.prsbipolarlow)$conf.int[c(3)],
                   summary(cox.prsbipolarhigh)$conf.int[c(3)],
                   NA,
                   summary(cox.prsschizophrenialow)$conf.int[c(3)],
                   summary(cox.prsschizophreniahigh)$conf.int[c(3)],
                   NA,
                   summary(cox.prsadhdlow)$conf.int[c(3)],
                   summary(cox.prsadhdhigh)$conf.int[c(3)]
                  )
forest.res$ucl <- c(summary(cox)$conf.int[c(4)],
                   NA,
                   summary(cox.genderMale)$conf.int[c(4)],
                   summary(cox.genderOther)$conf.int[c(4)],
                   NA,
                   summary(cox.35)$conf.int[c(4)],
                   summary(cox.3555)$conf.int[c(4)],
                   summary(cox.55)$conf.int[c(4)],
                   NA,
                   summary(cox.smoker0)$conf.int[c(4)],
                   summary(cox.smoker1)$conf.int[c(4)],
                   NA,
                   summary(cox.prsdepressionlow)$conf.int[c(4)],
                   summary(cox.prsdepressionhigh)$conf.int[c(4)],
                   NA,
                   summary(cox.prssuilow)$conf.int[c(4)],
                   summary(cox.prssuihigh)$conf.int[c(4)],
                   NA,
                   summary(cox.prsanxietylow)$conf.int[c(4)],
                   summary(cox.prsanxietyhigh)$conf.int[c(4)],
                   NA,
                   summary(cox.prsbipolarlow)$conf.int[c(4)],
                   summary(cox.prsbipolarhigh)$conf.int[c(4)],
                   NA,
                   summary(cox.prsschizophrenialow)$conf.int[c(4)],
                   summary(cox.prsschizophreniahigh)$conf.int[c(4)],
                   NA,
                   summary(cox.prsadhdlow)$conf.int[c(4)],
                   summary(cox.prsadhdhigh)$conf.int[c(4)]
                  )

forest.res <- as.data.frame(forest.res)
forest.res$'                         Hazard ratio' <- paste(rep("", nrow(forest.res)), collapse = "   ")
forest.res$'P-for-\ninteraction' <- c('',
                                round2(P.gender,2),'','',
                                '',round2(P.age.35,2),round2(P.age.3555,2),round2(P.age.55,2),
                                round2(P.smoker,2),'','',
                                round2(P.prsdepression,2),'','',
                                round2(P.prssui,2),'','',
                                round2(P.prsanxiety,2),'','',
                                round2(P.prsbipolar,2),'','',
                                round2(P.prsschizophrenia,2),'','',
                                round2(P.prsadhd,2),'','')
forest.res

p <- forest(forest.res[,c(1,5,6)],
            est = forest.res$est,
            lower = forest.res$lcl, 
            upper = forest.res$ucl,
            ci_column = 2,
            ref_line = 1,
            arrow_lab = c("SSRI Better","Bupropion Better"),
            xlim = c(0, 2.5),
            ticks_at = c(0.5,1,1.5,2)
           )

# Print plot
options(repr.plot.width=13, repr.plot.height=8)
plot(p)
options(repr.plot.width=7, repr.plot.height=7)