library(dplyr)
library(tidyr)
library(ggplot2)
library(arsenal)
library(fastDummies)
library(stringr)
library(purrr)
library(MatchIt)
library(data.table)

clean_data_ssri_bupropion <- function(data,dataset_drug_df,dataset_condition_df,dataset_survey_df,drug_treatment,drug_control,seed,redo=FALSE) {
  
  datafile <- "clean_data_ssri_bupropion.Rda"
  
  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call clean(redo=TRUE) to update data.\n")
    load(datafile)
    return(list(sample_match))
  }
  
  set.seed(seed)
  
  print("Creating variables in original dataframes ...")
  flush.console()
  print("Variables chosen based on previous EHR prediction model for suicide ...")
  flush.console()
  print("https://ajp.psychiatryonline.org/doi/full/10.1176/appi.ajp.2018.17101167")
  flush.console()
  
  #drug class
  dataset_drug_df$tca = ifelse(str_detect(dataset_drug_df$standard_concept_name,regex("Amitriptyline|Amoxapine|Clomipramine|Desipramine|Doxepin|Imipramine|Maprotiline|Nortriptyline|Protriptyline|Trimipramine",ignore_case=TRUE)), 1, 0)
  dataset_drug_df$ssri = ifelse(str_detect(dataset_drug_df$standard_concept_name,regex("Citalopram|Escitalopram|Fluoxetine|Fluvoxamine|Paroxetine|Sertraline",ignore_case=TRUE)), 1, 0)
  dataset_drug_df$snri = ifelse(str_detect(dataset_drug_df$standard_concept_name,regex("Desvenlafaxine|Duloxetine|Milnacipran|Venlafaxine",ignore_case=TRUE)), 1, 0)
  dataset_drug_df$bupropion = ifelse(str_detect(dataset_drug_df$standard_concept_name,regex("bupropion",ignore_case=TRUE)), 1, 0)
  
  #conditions: need to double check!
  dataset_condition_df$sui <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("Thoughts of self harm|Suicidal thoughts|Planning suicide|Suicidal intent|Thoughts of deliberate self harm",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$anxiety <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("Generalized anxiety disorder",ignore_case=TRUE)), 1, 0)
  # dataset_condition_df$bipolar <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("bipolar",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$alcohol <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("alcohol abuse",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$substance <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("amphetamine abuse|cocaine abuse|cannabis abuse",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$schizophrenia <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("schizophrenia",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$eating <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("anorexia nervosa|bulimia nervosa",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$ptsd <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("Posttraumatic stress disorder|post-traumatic stress disorder",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$adhd <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("Attention deficit hyperactivity disorder",ignore_case=TRUE)), 1, 0)
  
  chooserand <- function(x) {
    if (length(x) == 1) {
      return(x)
    } else {
      return(sample(x, 1))
    }
  }
  
  dataset_survey <- as.data.table(dataset_survey_df)
  rm(dataset_survey_df)
  dataset_condition <- as.data.table(dataset_condition_df)
  rm(dataset_condition_df)
  dataset_drug <- as.data.table(dataset_drug_df)
  rm(dataset_drug_df)
  data_demographics <- as.data.table(data)
  
  # Convert to Date
  dataset_condition[, condition_start_date := as.Date(substr(condition_start_datetime, 1, 10), origin = "1970-01-01")]
  dataset_drug[, drug_exposure_start_date := as.Date(substr(drug_exposure_start_datetime, 1, 10), origin = "1970-01-01")]
  
  
  print("Initializing data ...")
  flush.console()
  
  
  id_ssri <- unique(dataset_drug[dataset_drug[[drug_treatment]]==1,]$person_id)
  
  data_date_drug_ssri <- dataset_drug[
    person_id %in% id_ssri & ssri == 1,
    .SD[which.min(drug_exposure_start_date)], by = person_id
  ]
  
  
  id_bupropion <- unique(dataset_drug[dataset_drug[[drug_control]]==1,]$person_id)
  
  data_date_drug_bupropion <- dataset_drug[
    person_id %in% id_bupropion & bupropion == 1,
    .SD[which.min(drug_exposure_start_date)], by = person_id
  ]
  
  
  data_ssri <- data.table(
    person_id = data_date_drug_ssri$person_id,
    date_drug = data_date_drug_ssri$drug_exposure_start_date
  )
  
  data_bupropion <- data.table(
    person_id = data_date_drug_bupropion$person_id,
    date_drug = data_date_drug_bupropion$drug_exposure_start_date
  )
  
  # Perform a join to get both dates
  merged_data <- merge(data_ssri, data_bupropion, by = "person_id", all=TRUE, suffixes = c("_ssri", "_bupropion"))
  
  data_ssri <- data_ssri[person_id %in% merged_data[date_drug_ssri < date_drug_bupropion | is.na(date_drug_bupropion)]$person_id]
  data_bupropion <- data_bupropion[person_id %in% merged_data[date_drug_bupropion < date_drug_ssri | is.na(date_drug_ssri)]$person_id]
  
  id_ssri <- data_ssri$person_id
  id_bupropion <- data_bupropion$person_id
  
  
  
  print("Creating data_ssri ...")
  flush.console()


  suppressWarnings({
    
    print("Survey ...")
    flush.console()
    
    # Merge with survey data to get smoking status
    smoker_data <- dataset_survey[
      person_id %in% id_ssri & question == "Smoking: 100 Cigs Lifetime",
      .(person_id, answer)]
    
    smoker_data[, smoker := case_when(
      answer %in% c("PMI: Dont Know", "PMI: Prefer Not To Answer", "PMI: Skip") ~ NA_real_,
      answer == "100 Cigs Lifetime: Yes" ~ 1,
      answer == "100 Cigs Lifetime: No" ~ 0
    )]
    
    data_ssri <- merge(data_ssri, smoker_data[, .(person_id, smoker)], by = "person_id", all.x = TRUE)
    
    
    # Merge with survey data to get income
    income_data <- dataset_survey[
      person_id %in% id_ssri & question == "Income: Annual Income",
      .(person_id, answer)]
    
    income_data[, income := case_when( 
      answer %in% c("PMI: Prefer Not To Answer", "PMI: Skip") ~ NA, 
      answer %in% c("Annual Income: less 10k", "Annual Income: 10k 25k") 
      ~ "-25k", 
      answer %in% c("Annual Income: 25k 35k", "Annual Income: 35k 50k")
      ~ "25k-50k", 
      answer %in% c("Annual Income: 50k 75k", "Annual Income: 75k 100k")
      ~ "50k-100k", 
      answer %in% c("Annual Income: 100k 150k", "Annual Income: 150k 200k", "Annual Income: more 200k")
      ~ "100k-" 
    )]
    
    data_ssri <- merge(data_ssri, income_data[, .(person_id, income)], by = "person_id", all.x = TRUE)
    
    # Merge with survey data to get insurance
    insurance_data <- dataset_survey[
      person_id %in% id_ssri & question == "Insurance: Health Insurance",
      .(person_id, answer)]
    
    insurance_data[, insurance := case_when( 
      answer %in% c("PMI: Prefer Not To Answer", "PMI: Skip", "PMI: Dont Know") ~ NA, 
      answer == "Health Insurance: Yes" ~ 1, 
      answer == "Health Insurance: No" ~ 0 
    )]
    
    data_ssri <- merge(data_ssri, insurance_data[, .(person_id, insurance)], by = "person_id", all.x = TRUE)
    
    # Merge with survey data to get edu
    edu_data <- dataset_survey[
      person_id %in% id_ssri & question == "Education Level: Highest Grade",
      .(person_id, answer)]
    edu_data[, edu := case_when( 
      answer %in% c("PMI: Prefer Not To Answer", "PMI: Skip") ~ NA, 
      answer %in% c("Highest Grade: Never Attended", 
                    "Highest Grade: One Through Four",
                    "Highest Grade: Five Through Eight",
                    "Highest Grade: Nine Through Eleven",
                    "Highest Grade: Twelve Or GED")
      ~ "-High school grad (Twelve or GED)", 
      answer %in% c("Highest Grade: College One to Three", 
                    "Highest Grade: College Graduate", 
                    "Highest Grade: Advanced Degree")
      ~ "Attended college or higher" 
    )]
    
    
    data_ssri <- merge(data_ssri, edu_data[, .(person_id, edu)], by = "person_id", all.x = TRUE)
    
    # Merge with survey data to get birthplace
    birthplace_data <- dataset_survey[
      person_id %in% id_ssri & question == "The Basics: Birthplace",
      .(person_id, answer)]
    
    birthplace_data[, birthplace := case_when( 
      answer == "PMI: Skip" ~ NA, 
      answer == "Birthplace: USA" ~ 1, 
      answer == "PMI: Other" ~ 0
    )]
    
    data_ssri <- merge(data_ssri, birthplace_data[, .(person_id, birthplace)], by = "person_id", all.x = TRUE)
    
    
    
    print("Conditions ...")
    flush.console()
    
    # functions for comparing dates
    compare_dates <- function(dates, drug_date) {
      any(sapply(dates, function(date) date > drug_date))
    }
    compare_dates_pre <- function(dates, drug_date) {
      any(sapply(dates, function(date) date <= drug_date))
    }
    compare_dates_3 <- function(dates, drug_date) {
      any(sapply(dates, function(date) date > drug_date & date - drug_date <= 365*3))
    }
    compare_dates_5 <- function(dates, drug_date) {
      any(sapply(dates, function(date) date > drug_date & date - drug_date <= 365*5))
    }
    compare_dates_pmh <- function(dates, drug_date) {
      any(sapply(dates, function(date) date <= drug_date))
    }
    compare_dates_time <- function(dates, drug_date) {
      any(sapply(dates, function(date) date > drug_date))
    }
    compare_dates_time_yes <- function(dates, drug_date) {
      min(sapply(dates, function(date) date - drug_date)[sapply(dates, function(date) date - drug_date) > 0])
    }
    compare_dates_time_no <- function(drug_date) {
      as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - drug_date)
    }
    
    #sui
    sui_data <- dataset_condition[person_id %in% id_ssri & sui == 1]
    
    sui_info <- sui_data[, .(
      sui_dates = list(condition_start_date)
    ), by = person_id]
    
    sui_info <- merge(sui_info, data_ssri[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    
    sui_info[, `:=`(
      sui = fifelse(mapply(compare_dates, sui_dates, date_drug), 1, 0),
      sui_3 = fifelse(mapply(compare_dates_3, sui_dates, date_drug), 1, 0),
      sui_5 = fifelse(mapply(compare_dates_5, sui_dates, date_drug), 1, 0),
      sui_pmh = fifelse(mapply(compare_dates_pmh, sui_dates, date_drug), 1, 0),
      sui_time = fifelse(mapply(compare_dates_time, sui_dates, date_drug),
                             mapply(compare_dates_time_yes, sui_dates, date_drug),
                             mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_ssri table
    data_ssri <- merge(data_ssri, sui_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_ssri[, sui := fifelse(is.na(sui), 0, sui)]
    data_ssri[, sui_3 := fifelse(is.na(sui_3), 0, sui_3)]
    data_ssri[, sui_5 := fifelse(is.na(sui_5), 0, sui_5)]
    data_ssri[, sui_pmh := fifelse(is.na(sui_pmh), 0, sui_pmh)]
    data_ssri[, sui_time := fifelse(is.na(sui_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), sui_time)]
    
    rm(sui_data, sui_info)
    
    
    #anxiety
    anxiety_data <- dataset_condition[person_id %in% id_ssri & anxiety == 1]
    
    anxiety_info <- anxiety_data[, .(
      anxiety_dates = list(condition_start_date)
    ), by = person_id]
    
    anxiety_info <- merge(anxiety_info, data_ssri[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    
    anxiety_info[, `:=`(
      anxiety = fifelse(mapply(compare_dates, anxiety_dates, date_drug), 1, 0),
      anxiety_3 = fifelse(mapply(compare_dates_3, anxiety_dates, date_drug), 1, 0),
      anxiety_5 = fifelse(mapply(compare_dates_5, anxiety_dates, date_drug), 1, 0),
      anxiety_pmh = fifelse(mapply(compare_dates_pmh, anxiety_dates, date_drug), 1, 0),
      anxiety_time = fifelse(mapply(compare_dates_time, anxiety_dates, date_drug),
                                      mapply(compare_dates_time_yes, anxiety_dates, date_drug),
                                      mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_ssri table
    data_ssri <- merge(data_ssri, anxiety_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_ssri[, anxiety := fifelse(is.na(anxiety), 0, anxiety)]
    data_ssri[, anxiety_3 := fifelse(is.na(anxiety_3), 0, anxiety_3)]
    data_ssri[, anxiety_5 := fifelse(is.na(anxiety_5), 0, anxiety_5)]
    data_ssri[, anxiety_pmh := fifelse(is.na(anxiety_pmh), 0, anxiety_pmh)]
    data_ssri[, anxiety_time := fifelse(is.na(anxiety_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), anxiety_time)]
    
    rm(anxiety_data, anxiety_info)
    
    
    #alcohol
    alcohol_data <- dataset_condition[person_id %in% id_ssri & alcohol == 1]
    
    alcohol_info <- alcohol_data[, .(
      alcohol_dates = list(condition_start_date)
    ), by = person_id]
    
    alcohol_info <- merge(alcohol_info, data_ssri[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    alcohol_info[, `:=`(
      alcohol = fifelse(mapply(compare_dates, alcohol_dates, date_drug), 1, 0),
      alcohol_3 = fifelse(mapply(compare_dates_3, alcohol_dates, date_drug), 1, 0),
      alcohol_5 = fifelse(mapply(compare_dates_5, alcohol_dates, date_drug), 1, 0),
      alcohol_pmh = fifelse(mapply(compare_dates_pmh, alcohol_dates, date_drug), 1, 0),
      alcohol_time = fifelse(mapply(compare_dates_time, alcohol_dates, date_drug),
                             mapply(compare_dates_time_yes, alcohol_dates, date_drug),
                             mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_ssri table
    data_ssri <- merge(data_ssri, alcohol_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_ssri[, alcohol := fifelse(is.na(alcohol), 0, alcohol)]
    data_ssri[, alcohol_3 := fifelse(is.na(alcohol_3), 0, alcohol_3)]
    data_ssri[, alcohol_5 := fifelse(is.na(alcohol_5), 0, alcohol_5)]
    data_ssri[, alcohol_pmh := fifelse(is.na(alcohol_pmh), 0, alcohol_pmh)]
    data_ssri[, alcohol_time := fifelse(is.na(alcohol_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), alcohol_time)]
    
    rm(alcohol_data, alcohol_info)
    
    
    #substance
    substance_data <- dataset_condition[person_id %in% id_ssri & substance == 1]
    
    substance_info <- substance_data[, .(
      substance_dates = list(condition_start_date)
    ), by = person_id]
    
    substance_info <- merge(substance_info, data_ssri[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    substance_info[, `:=`(
      substance = fifelse(mapply(compare_dates, substance_dates, date_drug), 1, 0),
      substance_3 = fifelse(mapply(compare_dates_3, substance_dates, date_drug), 1, 0),
      substance_5 = fifelse(mapply(compare_dates_5, substance_dates, date_drug), 1, 0),
      substance_pmh = fifelse(mapply(compare_dates_pmh, substance_dates, date_drug), 1, 0),
      substance_time = fifelse(mapply(compare_dates_time, substance_dates, date_drug),
                               mapply(compare_dates_time_yes, substance_dates, date_drug),
                               mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_ssri table
    data_ssri <- merge(data_ssri, substance_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_ssri[, substance := fifelse(is.na(substance), 0, substance)]
    data_ssri[, substance_3 := fifelse(is.na(substance_3), 0, substance_3)]
    data_ssri[, substance_5 := fifelse(is.na(substance_5), 0, substance_5)]
    data_ssri[, substance_pmh := fifelse(is.na(substance_pmh), 0, substance_pmh)]
    data_ssri[, substance_time := fifelse(is.na(substance_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), substance_time)]
    
    rm(substance_data, substance_info)
    
    #schizophrenia
    schizophrenia_data <- dataset_condition[person_id %in% id_ssri & schizophrenia == 1]
    
    schizophrenia_info <- schizophrenia_data[, .(
      schizophrenia_dates = list(condition_start_date)
    ), by = person_id]
    
    schizophrenia_info <- merge(schizophrenia_info, data_ssri[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    schizophrenia_info[, `:=`(
      schizophrenia = fifelse(mapply(compare_dates, schizophrenia_dates, date_drug), 1, 0),
      schizophrenia_3 = fifelse(mapply(compare_dates_3, schizophrenia_dates, date_drug), 1, 0),
      schizophrenia_5 = fifelse(mapply(compare_dates_5, schizophrenia_dates, date_drug), 1, 0),
      schizophrenia_pmh = fifelse(mapply(compare_dates_pmh, schizophrenia_dates, date_drug), 1, 0),
      schizophrenia_time = fifelse(mapply(compare_dates_time, schizophrenia_dates, date_drug),
                                   mapply(compare_dates_time_yes, schizophrenia_dates, date_drug),
                                   mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_ssri table
    data_ssri <- merge(data_ssri, schizophrenia_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_ssri[, schizophrenia := fifelse(is.na(schizophrenia), 0, schizophrenia)]
    data_ssri[, schizophrenia_3 := fifelse(is.na(schizophrenia_3), 0, schizophrenia_3)]
    data_ssri[, schizophrenia_5 := fifelse(is.na(schizophrenia_5), 0, schizophrenia_5)]
    data_ssri[, schizophrenia_pmh := fifelse(is.na(schizophrenia_pmh), 0, schizophrenia_pmh)]
    data_ssri[, schizophrenia_time := fifelse(is.na(schizophrenia_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), schizophrenia_time)]
    
    rm(schizophrenia_data, schizophrenia_info)
    
    #eating
    eating_data <- dataset_condition[person_id %in% id_ssri & eating == 1]
    
    eating_info <- eating_data[, .(
      eating_dates = list(condition_start_date)
    ), by = person_id]
    
    eating_info <- merge(eating_info, data_ssri[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    eating_info[, `:=`(
      eating = fifelse(mapply(compare_dates, eating_dates, date_drug), 1, 0),
      eating_3 = fifelse(mapply(compare_dates_3, eating_dates, date_drug), 1, 0),
      eating_5 = fifelse(mapply(compare_dates_5, eating_dates, date_drug), 1, 0),
      eating_pmh = fifelse(mapply(compare_dates_pmh, eating_dates, date_drug), 1, 0),
      eating_time = fifelse(mapply(compare_dates_time, eating_dates, date_drug),
                            mapply(compare_dates_time_yes, eating_dates, date_drug),
                            mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_ssri table
    data_ssri <- merge(data_ssri, eating_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_ssri[, eating := fifelse(is.na(eating), 0, eating)]
    data_ssri[, eating_3 := fifelse(is.na(eating_3), 0, eating_3)]
    data_ssri[, eating_5 := fifelse(is.na(eating_5), 0, eating_5)]
    data_ssri[, eating_pmh := fifelse(is.na(eating_pmh), 0, eating_pmh)]
    data_ssri[, eating_time := fifelse(is.na(eating_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), eating_time)]
    
    rm(eating_data, eating_info)
    
    #ptsd
    ptsd_data <- dataset_condition[person_id %in% id_ssri & ptsd == 1]
    
    ptsd_info <- ptsd_data[, .(
      ptsd_dates = list(condition_start_date)
    ), by = person_id]
    
    ptsd_info <- merge(ptsd_info, data_ssri[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    ptsd_info[, `:=`(
      ptsd = fifelse(mapply(compare_dates, ptsd_dates, date_drug), 1, 0),
      ptsd_3 = fifelse(mapply(compare_dates_3, ptsd_dates, date_drug), 1, 0),
      ptsd_5 = fifelse(mapply(compare_dates_5, ptsd_dates, date_drug), 1, 0),
      ptsd_pmh = fifelse(mapply(compare_dates_pmh, ptsd_dates, date_drug), 1, 0),
      ptsd_time = fifelse(mapply(compare_dates_time, ptsd_dates, date_drug),
                            mapply(compare_dates_time_yes, ptsd_dates, date_drug),
                            mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_ssri table
    data_ssri <- merge(data_ssri, ptsd_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_ssri[, ptsd := fifelse(is.na(ptsd), 0, ptsd)]
    data_ssri[, ptsd_3 := fifelse(is.na(ptsd_3), 0, ptsd_3)]
    data_ssri[, ptsd_5 := fifelse(is.na(ptsd_5), 0, ptsd_5)]
    data_ssri[, ptsd_pmh := fifelse(is.na(ptsd_pmh), 0, ptsd_pmh)]
    data_ssri[, ptsd_time := fifelse(is.na(ptsd_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), ptsd_time)]
    
    rm(ptsd_data, ptsd_info)
    
    #adhd
    adhd_data <- dataset_condition[person_id %in% id_ssri & adhd == 1]
    
    adhd_info <- adhd_data[, .(
      adhd_dates = list(condition_start_date)
    ), by = person_id]
    
    adhd_info <- merge(adhd_info, data_ssri[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    adhd_info[, `:=`(
      adhd = fifelse(mapply(compare_dates, adhd_dates, date_drug), 1, 0),
      adhd_3 = fifelse(mapply(compare_dates_3, adhd_dates, date_drug), 1, 0),
      adhd_5 = fifelse(mapply(compare_dates_5, adhd_dates, date_drug), 1, 0),
      adhd_pmh = fifelse(mapply(compare_dates_pmh, adhd_dates, date_drug), 1, 0),
      adhd_time = fifelse(mapply(compare_dates_time, adhd_dates, date_drug),
                          mapply(compare_dates_time_yes, adhd_dates, date_drug),
                          mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_ssri table
    data_ssri <- merge(data_ssri, adhd_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_ssri[, adhd := fifelse(is.na(adhd), 0, adhd)]
    data_ssri[, adhd_3 := fifelse(is.na(adhd_3), 0, adhd_3)]
    data_ssri[, adhd_5 := fifelse(is.na(adhd_5), 0, adhd_5)]
    data_ssri[, adhd_pmh := fifelse(is.na(adhd_pmh), 0, adhd_pmh)]
    data_ssri[, adhd_time := fifelse(is.na(adhd_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), adhd_time)]
    
    rm(adhd_data, adhd_info)
    
    
    print("Drugs ...")
    flush.console()
    
    # Process drug data: tca                           
    tca_data <- dataset_drug[person_id %in% id_ssri & tca == 1]
    
    tca_info <- tca_data[, .(
      tca_dates = list(drug_exposure_start_date)
    ), by = person_id]
    
    tca_info <- merge(tca_info, data_ssri[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    tca_info[,tca_pre := fifelse(mapply(compare_dates_pre, tca_dates, date_drug), 1, 0)]
    
    # Merge with the main data_ssri table
    data_ssri <- merge(data_ssri, tca_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_ssri[, tca_pre := fifelse(is.na(tca_pre), 0, tca_pre)]
    
    rm(tca_data, tca_info)
    
  })
  
  
  print("Demographics ...")
  flush.console()
  
  # Process demographic variables
  data_demographics_ssri <- data_demographics[person_id %in% id_ssri]
  
  data_demographics_ssri[, date_of_birth := as.Date(substr(date_of_birth, 1, 10), origin = "1970-01-01")]
  
  data_ssri <- merge(data_ssri, data_demographics_ssri[, .(
    person_id,
    date_of_birth,
    gender = case_when(
      gender %in% c("I prefer not to answer", "PMI: Skip", "Not man only, not woman only, prefer not to answer, or skipped") ~ NA,
      gender == "Female" ~ "Female",
      gender == "Male" ~ "Male",
      gender == "Gender Identity: Transgender" ~ "Other",
      gender == "Gender Identity: Non Binary" ~ "Other",
      gender == "Gender Identity: Additional Options" ~ "Other"
    ),
    race = case_when(
      race %in% c("I prefer not to answer", "PMI: Skip") ~ NA,
      race == "Asian" ~ "Other",
      race == "Black or African American" ~ "Black",
      race == "Middle Eastern or North African" ~ "Other",
      race == "More than one population" ~ "Other",
      race == "Native Hawaiian or Other Pacific Islander" ~ "Other",
      race %in% c("None Indicated", "None of these") ~ "Other",
      race == "White" ~ "White"
    ),
    ethnicity = case_when(
      ethnicity %in% c("PMI: Prefer Not To Answer", "PMI: Skip") ~ NA,
      ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
      ethnicity == "Not Hispanic or Latino" ~ "Not Hispanic or Latino",
      ethnicity == "What Race Ethnicity: Race Ethnicity None Of These" ~ "Not Hispanic or Latino"
    ),
    sex_at_birth = case_when(
      sex_at_birth == "Female" ~ "Female",
      sex_at_birth %in% c("I prefer not to answer", "PMI: Skip") ~ NA,
      sex_at_birth %in% c("Intersex", "Male", "No matching concept", "None") ~ "Male & other"
    )
  )], by = "person_id", all.x = TRUE)
  
  data_ssri[, `:=`(
    rande = case_when(
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      ethnicity == "Not Hispanic or Latino" & race == "White" ~ "Non-Hispanic White",
      ethnicity == "Not Hispanic or Latino" & race == "Black" ~ "Non-Hispanic Black",
      ethnicity == "Not Hispanic or Latino" & race == "Other" ~ "Other"
    )
  )]
  
  data_ssri[, age := round(as.numeric(date_drug - date_of_birth)/365)]
  
  data_ssri = as.data.frame(data_ssri)
  
  data_ssri$arm <- 1
  
  data_ssri <- data_ssri[c("person_id","date_drug","sui","sui_3","sui_5","sui_time","arm","gender","age","rande","edu","income","insurance","birthplace","smoker","sui_pmh","anxiety_pmh","alcohol_pmh","substance_pmh","schizophrenia_pmh","eating_pmh","ptsd_pmh","adhd_pmh","tca_pre")]
  
  print("Full ssri data")                                                      
  print(summary(data_ssri))
  
  
  data_ssri <- data_ssri[complete.cases(data_ssri),] 
  
  print("Complete cases, ssri")                                                      
  print(summary(data_ssri))
  

  
  print("Creating data_bupropion ...")
  flush.console()
  
  
  suppressWarnings({
    
    print("Survey ...")
    flush.console()
    
    # Merge with survey data to get smoking status
    smoker_data <- dataset_survey[
      person_id %in% id_bupropion & question == "Smoking: 100 Cigs Lifetime",
      .(person_id, answer)]
    
    smoker_data[, smoker := case_when(
      answer %in% c("PMI: Dont Know", "PMI: Prefer Not To Answer", "PMI: Skip") ~ NA_real_,
      answer == "100 Cigs Lifetime: Yes" ~ 1,
      answer == "100 Cigs Lifetime: No" ~ 0
    )]
    
    data_bupropion <- merge(data_bupropion, smoker_data[, .(person_id, smoker)], by = "person_id", all.x = TRUE)
    
    
    # Merge with survey data to get income
    income_data <- dataset_survey[
      person_id %in% id_bupropion & question == "Income: Annual Income",
      .(person_id, answer)]
    
    income_data[, income := case_when( 
      answer %in% c("PMI: Prefer Not To Answer", "PMI: Skip") ~ NA, 
      answer %in% c("Annual Income: less 10k", "Annual Income: 10k 25k") 
      ~ "-25k", 
      answer %in% c("Annual Income: 25k 35k", "Annual Income: 35k 50k")
      ~ "25k-50k", 
      answer %in% c("Annual Income: 50k 75k", "Annual Income: 75k 100k")
      ~ "50k-100k", 
      answer %in% c("Annual Income: 100k 150k", "Annual Income: 150k 200k", "Annual Income: more 200k")
      ~ "100k-" 
    )]
    
    data_bupropion <- merge(data_bupropion, income_data[, .(person_id, income)], by = "person_id", all.x = TRUE)
    
    # Merge with survey data to get insurance
    insurance_data <- dataset_survey[
      person_id %in% id_bupropion & question == "Insurance: Health Insurance",
      .(person_id, answer)]
    
    insurance_data[, insurance := case_when( 
      answer %in% c("PMI: Prefer Not To Answer", "PMI: Skip", "PMI: Dont Know") ~ NA, 
      answer == "Health Insurance: Yes" ~ 1, 
      answer == "Health Insurance: No" ~ 0 
    )]
    
    data_bupropion <- merge(data_bupropion, insurance_data[, .(person_id, insurance)], by = "person_id", all.x = TRUE)
    
    # Merge with survey data to get edu
    edu_data <- dataset_survey[
      person_id %in% id_bupropion & question == "Education Level: Highest Grade",
      .(person_id, answer)]
    
    edu_data[, edu := case_when( 
      answer %in% c("PMI: Prefer Not To Answer", "PMI: Skip") ~ NA, 
      answer %in% c("Highest Grade: Never Attended", 
                    "Highest Grade: One Through Four",
                    "Highest Grade: Five Through Eight",
                    "Highest Grade: Nine Through Eleven",
                    "Highest Grade: Twelve Or GED")
      ~ "-High school grad (Twelve or GED)", 
      answer %in% c("Highest Grade: College One to Three", 
                    "Highest Grade: College Graduate", 
                    "Highest Grade: Advanced Degree")
      ~ "Attended college or higher" 
    )]
    
    data_bupropion <- merge(data_bupropion, edu_data[, .(person_id, edu)], by = "person_id", all.x = TRUE)
    
    # Merge with survey data to get birthplace
    birthplace_data <- dataset_survey[
      person_id %in% id_bupropion & question == "The Basics: Birthplace",
      .(person_id, answer)]
    
    birthplace_data[, birthplace := case_when( 
      answer == "PMI: Skip" ~ NA, 
      answer == "Birthplace: USA" ~ 1, 
      answer == "PMI: Other" ~ 0
    )]
    
    data_bupropion <- merge(data_bupropion, birthplace_data[, .(person_id, birthplace)], by = "person_id", all.x = TRUE)
    
    
    
    print("Conditions ...")
    flush.console()
    
    # functions for comparing dates
    compare_dates <- function(dates, drug_date) {
      any(sapply(dates, function(date) date > drug_date))
    }
    compare_dates_pre <- function(dates, drug_date) {
      any(sapply(dates, function(date) date <= drug_date))
    }
    compare_dates_3 <- function(dates, drug_date) {
      any(sapply(dates, function(date) date > drug_date & date - drug_date <= 365*3))
    }
    compare_dates_5 <- function(dates, drug_date) {
      any(sapply(dates, function(date) date > drug_date & date - drug_date <= 365*5))
    }
    compare_dates_pmh <- function(dates, drug_date) {
      any(sapply(dates, function(date) date <= drug_date))
    }
    compare_dates_time <- function(dates, drug_date) {
      any(sapply(dates, function(date) date > drug_date))
    }
    compare_dates_time_yes <- function(dates, drug_date) {
      min(sapply(dates, function(date) date - drug_date)[sapply(dates, function(date) date - drug_date) > 0])
    }
    compare_dates_time_no <- function(drug_date) {
      as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - drug_date)
    }
    
    #sui
    sui_data <- dataset_condition[person_id %in% id_bupropion & sui == 1]
    
    sui_info <- sui_data[, .(
      sui_dates = list(condition_start_date)
    ), by = person_id]
    
    sui_info <- merge(sui_info, data_bupropion[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    
    sui_info[, `:=`(
      sui = fifelse(mapply(compare_dates, sui_dates, date_drug), 1, 0),
      sui_3 = fifelse(mapply(compare_dates_3, sui_dates, date_drug), 1, 0),
      sui_5 = fifelse(mapply(compare_dates_5, sui_dates, date_drug), 1, 0),
      sui_pmh = fifelse(mapply(compare_dates_pmh, sui_dates, date_drug), 1, 0),
      sui_time = fifelse(mapply(compare_dates_time, sui_dates, date_drug),
                         mapply(compare_dates_time_yes, sui_dates, date_drug),
                         mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_bupropion table
    data_bupropion <- merge(data_bupropion, sui_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_bupropion[, sui := fifelse(is.na(sui), 0, sui)]
    data_bupropion[, sui_3 := fifelse(is.na(sui_3), 0, sui_3)]
    data_bupropion[, sui_5 := fifelse(is.na(sui_5), 0, sui_5)]
    data_bupropion[, sui_pmh := fifelse(is.na(sui_pmh), 0, sui_pmh)]
    data_bupropion[, sui_time := fifelse(is.na(sui_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), sui_time)]
    
    rm(sui_data, sui_info)
    
    
    #anxiety
    anxiety_data <- dataset_condition[person_id %in% id_bupropion & anxiety == 1]
    
    anxiety_info <- anxiety_data[, .(
      anxiety_dates = list(condition_start_date)
    ), by = person_id]
    
    anxiety_info <- merge(anxiety_info, data_bupropion[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    
    anxiety_info[, `:=`(
      anxiety = fifelse(mapply(compare_dates, anxiety_dates, date_drug), 1, 0),
      anxiety_3 = fifelse(mapply(compare_dates_3, anxiety_dates, date_drug), 1, 0),
      anxiety_5 = fifelse(mapply(compare_dates_5, anxiety_dates, date_drug), 1, 0),
      anxiety_pmh = fifelse(mapply(compare_dates_pmh, anxiety_dates, date_drug), 1, 0),
      anxiety_time = fifelse(mapply(compare_dates_time, anxiety_dates, date_drug),
                             mapply(compare_dates_time_yes, anxiety_dates, date_drug),
                             mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_bupropion table
    data_bupropion <- merge(data_bupropion, anxiety_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_bupropion[, anxiety := fifelse(is.na(anxiety), 0, anxiety)]
    data_bupropion[, anxiety_3 := fifelse(is.na(anxiety_3), 0, anxiety_3)]
    data_bupropion[, anxiety_5 := fifelse(is.na(anxiety_5), 0, anxiety_5)]
    data_bupropion[, anxiety_pmh := fifelse(is.na(anxiety_pmh), 0, anxiety_pmh)]
    data_bupropion[, anxiety_time := fifelse(is.na(anxiety_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), anxiety_time)]
    
    rm(anxiety_data, anxiety_info)
    
    
    
    #alcohol
    alcohol_data <- dataset_condition[person_id %in% id_bupropion & alcohol == 1]
    
    alcohol_info <- alcohol_data[, .(
      alcohol_dates = list(condition_start_date)
    ), by = person_id]
    
    alcohol_info <- merge(alcohol_info, data_bupropion[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    alcohol_info[, `:=`(
      alcohol = fifelse(mapply(compare_dates, alcohol_dates, date_drug), 1, 0),
      alcohol_3 = fifelse(mapply(compare_dates_3, alcohol_dates, date_drug), 1, 0),
      alcohol_5 = fifelse(mapply(compare_dates_5, alcohol_dates, date_drug), 1, 0),
      alcohol_pmh = fifelse(mapply(compare_dates_pmh, alcohol_dates, date_drug), 1, 0),
      alcohol_time = fifelse(mapply(compare_dates_time, alcohol_dates, date_drug),
                             mapply(compare_dates_time_yes, alcohol_dates, date_drug),
                             mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_bupropion table
    data_bupropion <- merge(data_bupropion, alcohol_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_bupropion[, alcohol := fifelse(is.na(alcohol), 0, alcohol)]
    data_bupropion[, alcohol_3 := fifelse(is.na(alcohol_3), 0, alcohol_3)]
    data_bupropion[, alcohol_5 := fifelse(is.na(alcohol_5), 0, alcohol_5)]
    data_bupropion[, alcohol_pmh := fifelse(is.na(alcohol_pmh), 0, alcohol_pmh)]
    data_bupropion[, alcohol_time := fifelse(is.na(alcohol_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), alcohol_time)]
    
    rm(alcohol_data, alcohol_info)
    
    
    #substance
    substance_data <- dataset_condition[person_id %in% id_bupropion & substance == 1]
    
    substance_info <- substance_data[, .(
      substance_dates = list(condition_start_date)
    ), by = person_id]
    
    substance_info <- merge(substance_info, data_bupropion[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    substance_info[, `:=`(
      substance = fifelse(mapply(compare_dates, substance_dates, date_drug), 1, 0),
      substance_3 = fifelse(mapply(compare_dates_3, substance_dates, date_drug), 1, 0),
      substance_5 = fifelse(mapply(compare_dates_5, substance_dates, date_drug), 1, 0),
      substance_pmh = fifelse(mapply(compare_dates_pmh, substance_dates, date_drug), 1, 0),
      substance_time = fifelse(mapply(compare_dates_time, substance_dates, date_drug),
                               mapply(compare_dates_time_yes, substance_dates, date_drug),
                               mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_bupropion table
    data_bupropion <- merge(data_bupropion, substance_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_bupropion[, substance := fifelse(is.na(substance), 0, substance)]
    data_bupropion[, substance_3 := fifelse(is.na(substance_3), 0, substance_3)]
    data_bupropion[, substance_5 := fifelse(is.na(substance_5), 0, substance_5)]
    data_bupropion[, substance_pmh := fifelse(is.na(substance_pmh), 0, substance_pmh)]
    data_bupropion[, substance_time := fifelse(is.na(substance_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), substance_time)]
    
    rm(substance_data, substance_info)
    
    #schizophrenia
    schizophrenia_data <- dataset_condition[person_id %in% id_bupropion & schizophrenia == 1]
    
    schizophrenia_info <- schizophrenia_data[, .(
      schizophrenia_dates = list(condition_start_date)
    ), by = person_id]
    
    schizophrenia_info <- merge(schizophrenia_info, data_bupropion[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    schizophrenia_info[, `:=`(
      schizophrenia = fifelse(mapply(compare_dates, schizophrenia_dates, date_drug), 1, 0),
      schizophrenia_3 = fifelse(mapply(compare_dates_3, schizophrenia_dates, date_drug), 1, 0),
      schizophrenia_5 = fifelse(mapply(compare_dates_5, schizophrenia_dates, date_drug), 1, 0),
      schizophrenia_pmh = fifelse(mapply(compare_dates_pmh, schizophrenia_dates, date_drug), 1, 0),
      schizophrenia_time = fifelse(mapply(compare_dates_time, schizophrenia_dates, date_drug),
                                   mapply(compare_dates_time_yes, schizophrenia_dates, date_drug),
                                   mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_bupropion table
    data_bupropion <- merge(data_bupropion, schizophrenia_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_bupropion[, schizophrenia := fifelse(is.na(schizophrenia), 0, schizophrenia)]
    data_bupropion[, schizophrenia_3 := fifelse(is.na(schizophrenia_3), 0, schizophrenia_3)]
    data_bupropion[, schizophrenia_5 := fifelse(is.na(schizophrenia_5), 0, schizophrenia_5)]
    data_bupropion[, schizophrenia_pmh := fifelse(is.na(schizophrenia_pmh), 0, schizophrenia_pmh)]
    data_bupropion[, schizophrenia_time := fifelse(is.na(schizophrenia_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), schizophrenia_time)]
    
    rm(schizophrenia_data, schizophrenia_info)
    
    #eating
    eating_data <- dataset_condition[person_id %in% id_bupropion & eating == 1]
    
    eating_info <- eating_data[, .(
      eating_dates = list(condition_start_date)
    ), by = person_id]
    
    eating_info <- merge(eating_info, data_bupropion[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    eating_info[, `:=`(
      eating = fifelse(mapply(compare_dates, eating_dates, date_drug), 1, 0),
      eating_3 = fifelse(mapply(compare_dates_3, eating_dates, date_drug), 1, 0),
      eating_5 = fifelse(mapply(compare_dates_5, eating_dates, date_drug), 1, 0),
      eating_pmh = fifelse(mapply(compare_dates_pmh, eating_dates, date_drug), 1, 0),
      eating_time = fifelse(mapply(compare_dates_time, eating_dates, date_drug),
                            mapply(compare_dates_time_yes, eating_dates, date_drug),
                            mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_bupropion table
    data_bupropion <- merge(data_bupropion, eating_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_bupropion[, eating := fifelse(is.na(eating), 0, eating)]
    data_bupropion[, eating_3 := fifelse(is.na(eating_3), 0, eating_3)]
    data_bupropion[, eating_5 := fifelse(is.na(eating_5), 0, eating_5)]
    data_bupropion[, eating_pmh := fifelse(is.na(eating_pmh), 0, eating_pmh)]
    data_bupropion[, eating_time := fifelse(is.na(eating_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), eating_time)]
    
    rm(eating_data, eating_info)
    
    
    #ptsd
    ptsd_data <- dataset_condition[person_id %in% id_bupropion & ptsd == 1]
    
    ptsd_info <- ptsd_data[, .(
      ptsd_dates = list(condition_start_date)
    ), by = person_id]
    
    ptsd_info <- merge(ptsd_info, data_bupropion[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    ptsd_info[, `:=`(
      ptsd = fifelse(mapply(compare_dates, ptsd_dates, date_drug), 1, 0),
      ptsd_3 = fifelse(mapply(compare_dates_3, ptsd_dates, date_drug), 1, 0),
      ptsd_5 = fifelse(mapply(compare_dates_5, ptsd_dates, date_drug), 1, 0),
      ptsd_pmh = fifelse(mapply(compare_dates_pmh, ptsd_dates, date_drug), 1, 0),
      ptsd_time = fifelse(mapply(compare_dates_time, ptsd_dates, date_drug),
                            mapply(compare_dates_time_yes, ptsd_dates, date_drug),
                            mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_bupropion table
    data_bupropion <- merge(data_bupropion, ptsd_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_bupropion[, ptsd := fifelse(is.na(ptsd), 0, ptsd)]
    data_bupropion[, ptsd_3 := fifelse(is.na(ptsd_3), 0, ptsd_3)]
    data_bupropion[, ptsd_5 := fifelse(is.na(ptsd_5), 0, ptsd_5)]
    data_bupropion[, ptsd_pmh := fifelse(is.na(ptsd_pmh), 0, ptsd_pmh)]
    data_bupropion[, ptsd_time := fifelse(is.na(ptsd_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), ptsd_time)]
    
    rm(ptsd_data, ptsd_info)
    
    
    #adhd
    adhd_data <- dataset_condition[person_id %in% id_bupropion & adhd == 1]
    
    adhd_info <- adhd_data[, .(
      adhd_dates = list(condition_start_date)
    ), by = person_id]
    
    adhd_info <- merge(adhd_info, data_bupropion[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    adhd_info[, `:=`(
      adhd = fifelse(mapply(compare_dates, adhd_dates, date_drug), 1, 0),
      adhd_3 = fifelse(mapply(compare_dates_3, adhd_dates, date_drug), 1, 0),
      adhd_5 = fifelse(mapply(compare_dates_5, adhd_dates, date_drug), 1, 0),
      adhd_pmh = fifelse(mapply(compare_dates_pmh, adhd_dates, date_drug), 1, 0),
      adhd_time = fifelse(mapply(compare_dates_time, adhd_dates, date_drug),
                          mapply(compare_dates_time_yes, adhd_dates, date_drug),
                          mapply(compare_dates_time_no, date_drug))                   
    )]
    
    # Merge with the main data_bupropion table
    data_bupropion <- merge(data_bupropion, adhd_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_bupropion[, adhd := fifelse(is.na(adhd), 0, adhd)]
    data_bupropion[, adhd_3 := fifelse(is.na(adhd_3), 0, adhd_3)]
    data_bupropion[, adhd_5 := fifelse(is.na(adhd_5), 0, adhd_5)]
    data_bupropion[, adhd_pmh := fifelse(is.na(adhd_pmh), 0, adhd_pmh)]
    data_bupropion[, adhd_time := fifelse(is.na(adhd_time), as.numeric(as.Date("2022-07-01", origin = "1970-01-01") - date_drug), adhd_time)]
    
    rm(adhd_data, adhd_info)
    
    print("Drugs ...")
    flush.console()
    
    # Process drug data: tca                           
    tca_data <- dataset_drug[person_id %in% id_bupropion & tca == 1]
    
    tca_info <- tca_data[, .(
      tca_dates = list(drug_exposure_start_date)
    ), by = person_id]
    
    tca_info <- merge(tca_info, data_bupropion[,.(person_id,date_drug)], by = "person_id", all.x=TRUE)
    
    tca_info[,tca_pre := fifelse(mapply(compare_dates_pre, tca_dates, date_drug), 1, 0)]
    
    # Merge with the main data_bupropion table
    data_bupropion <- merge(data_bupropion, tca_info[,!("date_drug")], by = "person_id", all.x = TRUE)
    
    data_bupropion[, tca_pre := fifelse(is.na(tca_pre), 0, tca_pre)]
    
    rm(tca_data, tca_info)
    
  })
  
  
  print("Demographics ...")
  flush.console()
  
  # Process demographic variables
  data_demographics_bupropion <- data_demographics[person_id %in% id_bupropion]
  
  data_demographics_bupropion[, date_of_birth := as.Date(substr(date_of_birth, 1, 10), origin = "1970-01-01")]
  
  data_bupropion <- merge(data_bupropion, data_demographics_bupropion[, .(
    person_id,
    date_of_birth,
    gender = case_when(
      gender %in% c("I prefer not to answer", "PMI: Skip", "Not man only, not woman only, prefer not to answer, or skipped") ~ NA,
      gender == "Female" ~ "Female",
      gender == "Male" ~ "Male",
      gender == "Gender Identity: Transgender" ~ "Other",
      gender == "Gender Identity: Non Binary" ~ "Other",
      gender == "Gender Identity: Additional Options" ~ "Other"
    ),
    race = case_when(
      race %in% c("I prefer not to answer", "PMI: Skip") ~ NA,
      race == "Asian" ~ "Other",
      race == "Black or African American" ~ "Black",
      race == "Middle Eastern or North African" ~ "Other",
      race == "More than one population" ~ "Other",
      race == "Native Hawaiian or Other Pacific Islander" ~ "Other",
      race %in% c("None Indicated", "None of these") ~ "Other",
      race == "White" ~ "White"
    ),
    ethnicity = case_when(
      ethnicity %in% c("PMI: Prefer Not To Answer", "PMI: Skip") ~ NA,
      ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
      ethnicity == "Not Hispanic or Latino" ~ "Not Hispanic or Latino",
      ethnicity == "What Race Ethnicity: Race Ethnicity None Of These" ~ "Not Hispanic or Latino"
    ),
    sex_at_birth = case_when(
      sex_at_birth == "Female" ~ "Female",
      sex_at_birth %in% c("I prefer not to answer", "PMI: Skip") ~ NA,
      sex_at_birth %in% c("Intersex", "Male", "No matching concept", "None") ~ "Male & other"
    )
  )], by = "person_id", all.x = TRUE)
  
  data_bupropion[, `:=`(
    rande = case_when(
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      ethnicity == "Not Hispanic or Latino" & race == "White" ~ "Non-Hispanic White",
      ethnicity == "Not Hispanic or Latino" & race == "Black" ~ "Non-Hispanic Black",
      ethnicity == "Not Hispanic or Latino" & race == "Other" ~ "Other"
    )
  )]
  
  data_bupropion[, age := round(as.numeric(date_drug - date_of_birth)/365)]
  
  data_bupropion = as.data.frame(data_bupropion)
  
  data_bupropion$arm <- 0
  
  data_bupropion <- data_bupropion[c("person_id","date_drug","sui","sui_3","sui_5","sui_time","arm","gender","age","rande","edu","income","insurance","birthplace","smoker","sui_pmh","anxiety_pmh","alcohol_pmh","substance_pmh","schizophrenia_pmh","eating_pmh","ptsd_pmh","adhd_pmh","tca_pre")]
  
  print("Full bupropion data")                                                      
  print(summary(data_bupropion))
  
  
  data_bupropion <- data_bupropion[complete.cases(data_bupropion),] 
  
  print("Complete cases, bupropion")                                                      
  print(summary(data_bupropion))
  
  
  
  
  
  
  sample_match <- rbind(data_ssri,data_bupropion)
  save(sample_match,file=datafile)
  
  return(list(sample_match))
}

