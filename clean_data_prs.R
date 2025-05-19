library(dplyr)
library(tidyr)
library(ggplot2)
library(arsenal)
library(fastDummies)
library(stringr)
library(purrr)
library(MatchIt)
library(data.table)
library(rlang)

clean_data_prs <- function(data_prs,dataset_condition_df,seed,redo=FALSE) {
  
  datafile <- "clean_data_prs.Rda"
  
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
  
  #conditions: need to double check!
  dataset_condition_df$depression <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("Major depressive disorder|Major depression",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$sui <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("Thoughts of self harm|Suicidal thoughts|Planning suicide|Suicidal intent|Thoughts of deliberate self harm",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$anxiety <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("Generalized anxiety disorder",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$bipolar <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("bipolar",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$alcohol <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("alcohol abuse",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$substance <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("amphetamine abuse|cocaine abuse|cannabis abuse",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$schizophrenia <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("schizophrenia",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$eating <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("anorexia nervosa|bulimia nervosa",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$ptsd <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("Posttraumatic stress disorder|Chronic post-traumatic stress disorder",ignore_case=TRUE)), 1, 0)
  dataset_condition_df$adhd <- ifelse(str_detect(dataset_condition_df$standard_concept_name,regex("Attention deficit hyperactivity disorder",ignore_case=TRUE)), 1, 0)
  
  chooserand <- function(x) {
    if (length(x) == 1) {
      return(x)
    } else {
      return(sample(x, 1))
    }
  }
  
  dataset_condition <- as.data.table(dataset_condition_df)
  rm(dataset_condition_df)
  data_prs <- as.data.table(data_prs)
  
  # Convert to Date
  dataset_condition[, condition_start_date := as.Date(substr(condition_start_datetime, 1, 10), origin = "1970-01-01")]
  
  
  print("Creating data ...")
  flush.console()


  suppressWarnings({
    
    print("Conditions ...")
    flush.console()
    
    
    #depression
    depression_data <- dataset_condition[depression == 1]
    
    depression_info <- depression_data[, .(
      depression_dates = list(condition_start_date)
    ), by = person_id]
    
    depression_info[, depression := fifelse(mapply(is_empty, depression_dates), 0, 1)]
    
    # Merge with the main data_prs table
    data_prs <- merge(data_prs, depression_info, by = "person_id", all.x = TRUE)
    
    data_prs[, depression := fifelse(is.na(depression), 0, depression)]
    
    rm(depression_data, depression_info)
    
    #sui
    sui_data <- dataset_condition[sui == 1]
    
    sui_info <- sui_data[, .(
      sui_dates = list(condition_start_date)
    ), by = person_id]
    
    sui_info[, sui := fifelse(mapply(is_empty, sui_dates), 0, 1)]
    
    # Merge with the main data_prs table
    data_prs <- merge(data_prs, sui_info, by = "person_id", all.x = TRUE)
    
    data_prs[, sui := fifelse(is.na(sui), 0, sui)]
    
    rm(sui_data, sui_info)
    
    
    #anxiety
    anxiety_data <- dataset_condition[anxiety == 1]
    
    anxiety_info <- anxiety_data[, .(
      anxiety_dates = list(condition_start_date)
    ), by = person_id]
    
    anxiety_info[, anxiety := fifelse(mapply(is_empty, anxiety_dates), 0, 1)]
    
    # Merge with the main data_prs table
    data_prs <- merge(data_prs, anxiety_info, by = "person_id", all.x = TRUE)
    
    data_prs[, anxiety := fifelse(is.na(anxiety), 0, anxiety)]
    
    rm(anxiety_data, anxiety_info)
    
    
    #bipolar
    bipolar_data <- dataset_condition[bipolar == 1]
    
    bipolar_info <- bipolar_data[, .(
      bipolar_dates = list(condition_start_date)
    ), by = person_id]
    
    bipolar_info[, bipolar := fifelse(mapply(is_empty, bipolar_dates), 0, 1)]
    
    # Merge with the main data_prs table
    data_prs <- merge(data_prs, bipolar_info, by = "person_id", all.x = TRUE)
    
    data_prs[, bipolar := fifelse(is.na(bipolar), 0, bipolar)]
    
    rm(bipolar_data, bipolar_info)
    
    
    #alcohol
    alcohol_data <- dataset_condition[alcohol == 1]
    
    alcohol_info <- alcohol_data[, .(
      alcohol_dates = list(condition_start_date)
    ), by = person_id]
    
    alcohol_info[, alcohol := fifelse(mapply(is_empty, alcohol_dates), 0, 1)]
    
    # Merge with the main data_prs table
    data_prs <- merge(data_prs, alcohol_info, by = "person_id", all.x = TRUE)
    
    data_prs[, alcohol := fifelse(is.na(alcohol), 0, alcohol)]
    
    rm(alcohol_data, alcohol_info)
    
    
    
    #substance
    substance_data <- dataset_condition[substance == 1]
    
    substance_info <- substance_data[, .(
      substance_dates = list(condition_start_date)
    ), by = person_id]
    
    substance_info[, substance := fifelse(mapply(is_empty, substance_dates), 0, 1)]
    
    # Merge with the main data_prs table
    data_prs <- merge(data_prs, substance_info, by = "person_id", all.x = TRUE)
    
    data_prs[, substance := fifelse(is.na(substance), 0, substance)]
    
    rm(substance_data, substance_info)
    
    
    #schizophrenia
    schizophrenia_data <- dataset_condition[schizophrenia == 1]
    
    schizophrenia_info <- schizophrenia_data[, .(
      schizophrenia_dates = list(condition_start_date)
    ), by = person_id]
    
    schizophrenia_info[, schizophrenia := fifelse(mapply(is_empty, schizophrenia_dates), 0, 1)]
    
    # Merge with the main data_prs table
    data_prs <- merge(data_prs, schizophrenia_info, by = "person_id", all.x = TRUE)
    
    data_prs[, schizophrenia := fifelse(is.na(schizophrenia), 0, schizophrenia)]
    
    rm(schizophrenia_data, schizophrenia_info)
    
    #eating
    eating_data <- dataset_condition[eating == 1]
    
    eating_info <- eating_data[, .(
      eating_dates = list(condition_start_date)
    ), by = person_id]
    
    eating_info[, eating := fifelse(mapply(is_empty, eating_dates), 0, 1)]
    
    # Merge with the main data_prs table
    data_prs <- merge(data_prs, eating_info, by = "person_id", all.x = TRUE)
    
    data_prs[, eating := fifelse(is.na(eating), 0, eating)]
    
    rm(eating_data, eating_info)
    
    #ptsd
    ptsd_data <- dataset_condition[ptsd == 1]
    
    ptsd_info <- ptsd_data[, .(
      ptsd_dates = list(condition_start_date)
    ), by = person_id]
    
    ptsd_info[, ptsd := fifelse(mapply(is_empty, ptsd_dates), 0, 1)]
    
    # Merge with the main data_prs table
    data_prs <- merge(data_prs, ptsd_info, by = "person_id", all.x = TRUE)
    
    data_prs[, ptsd := fifelse(is.na(ptsd), 0, ptsd)]
    
    rm(ptsd_data, ptsd_info)
    
    
    
    #adhd
    adhd_data <- dataset_condition[adhd == 1]
    
    adhd_info <- adhd_data[, .(
      adhd_dates = list(condition_start_date)
    ), by = person_id]
    
    adhd_info[, adhd := fifelse(mapply(is_empty, adhd_dates), 0, 1)]
    
    # Merge with the main data_prs table
    data_prs <- merge(data_prs, adhd_info, by = "person_id", all.x = TRUE)
    
    data_prs[, adhd := fifelse(is.na(adhd), 0, adhd)]
    
    rm(adhd_data, adhd_info)
    
  })
  
  
  print("Demographics ...")
  flush.console()
  
  print(head(data_prs))
  
  # Process demographic variables
  data_prs[, `:=`(
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
    )
  )]
  
  data_prs[, `:=`(
    rande = case_when(
      ethnicity == "Hispanic or Latino" ~ "Hispanic",
      ethnicity == "Not Hispanic or Latino" & race == "White" ~ "Non-Hispanic White",
      ethnicity == "Not Hispanic or Latino" & race == "Black" ~ "Non-Hispanic Black",
      ethnicity == "Not Hispanic or Latino" & race == "Other" ~ "Other"
    )
  )]
  
  data_prs = as.data.frame(data_prs)
  
  data_prs <- data_prs[c("person_id","rande","depression","sui","anxiety","bipolar","alcohol","substance","schizophrenia","eating","ptsd","adhd")]
  
  print("Full data")                                                      
  print(summary(data_prs))
  
  data_prs <- data_prs[complete.cases(data_prs),] 
  
  print("Complete cases")                                                      
  print(summary(data_prs))

  

  save(data_prs,file=datafile)
  
  return(list(data_prs))
}

