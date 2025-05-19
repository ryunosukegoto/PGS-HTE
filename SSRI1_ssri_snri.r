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
libs <- c("doParallel","parallel","data.table","ggfortify","MatchIt","PooledCohort","grf","MatchIt","tableone","geepack","Information","missForest","doParallel","foreach","randomForest","itertools","doRNG","arsenal","fastDummies","parallel","stringr","purrr","naniar","dplyr","forestploter")
requireLibs(libs)
rm(requireLibs,libs)

source("extract_data_ssri.R")
datalist <- extract_data_ssri(redo=FALSE)

list2env(setNames(datalist, c("dataset_person_df", "dataset_drug_df", "dataset_condition_df", "dataset_survey_df")), envir = .GlobalEnv)

dataset_drug_df <- dataset_drug_df[c("person_id","standard_concept_name","drug_exposure_start_datetime")]
dataset_condition_df <- dataset_condition_df[c("person_id","standard_concept_name","condition_start_datetime")]
dataset_survey_df <- dataset_survey_df[c("person_id","question","answer")]

rm(datalist)

data <- data.frame(dataset_person_df)
drug_treatment <- "ssri"
drug_control <- "snri"
outcome <- "sui"
  
source("clean_data_ssri_snri.R")
data <- clean_data_ssri_snri(data,dataset_drug_df,dataset_condition_df,dataset_survey_df,drug_treatment,drug_control,seed=1
                           ,redo=TRUE)[[1]] #set redo=TRUE to update


head(data)

nrow(data)
summary(data)
table(data$arm)


#free up memory
rm(dataset_drug_df,dataset_condition_df,dataset_survey_df) 

#check that the latest date is before 2022-07-01
#cutoff for dates https://support.researchallofus.org/hc/en-us/articles/360051661772-What-are-the-CDR-cutoff-dates#:~:text=The%20current%20CDR%2C%20released%20in,2017%20%2D%20January%201%2C%202022.
summary(as.Date(data$date_drug,start_date="1970-01-01"))

bucket <- Sys.getenv("WORKSPACE_BUCKET")
write.csv(data, "data_ssri_snri.csv")
system(paste("gsutil cp", "data_ssri_snri.csv", paste0(bucket,"/data/","data_ssri_snri.csv")))

