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

source("extract_data_prs.R")
datalist <- extract_data_prs(redo=FALSE)

list2env(setNames(datalist, c("dataset_person_df", "dataset_condition_df", "dataset_survey_df")), envir = .GlobalEnv)

dataset_condition_df <- dataset_condition_df[c("person_id","standard_concept_name","condition_start_datetime")]
dataset_survey_df <- dataset_survey_df[c("person_id","question","answer")]

rm(datalist)

data <- data.frame(dataset_person_df)
  
source("clean_data_prs.R")
data <- clean_data_prs(data,dataset_condition_df,seed=1
                           ,redo=TRUE)[[1]] #set redo=TRUE to update


head(data)

nrow(data)
summary(data)


table(data$rande)

#free up memory
rm(dataset_condition_df,dataset_survey_df) 

bucket <- Sys.getenv("WORKSPACE_BUCKET")
write.csv(data, "data_prs.csv")
system(paste("gsutil cp", "data_prs.csv", paste0(bucket,"/data/","data_prs.csv")))

