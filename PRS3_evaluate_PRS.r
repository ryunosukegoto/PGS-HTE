requireLibs <- function(libs) {
  suppressWarnings({for(lib in libs){
      if(!require(lib, character.only = T)){
        install.packages(lib)
        require(lib, character.only = T)
      }
    }
  })
}
libs <- c("pROC","fmsb","doParallel","parallel","data.table","ggplot2")
requireLibs(libs)
rm(requireLibs,libs)

bucket <- Sys.getenv("WORKSPACE_BUCKET")
wd <- getwd()
system(paste("gsutil cp", paste0(bucket,"/data/data_prs.csv"), wd))
data_prs <- read.csv("data_prs.csv")

diseases = c("depression","sui","anxiety","bipolar","schizophrenia","adhd")

#load scores onto local folder

for(disease in diseases){
    file_path <- paste0(bucket,"/scores/",disease,"_scores_nostd.csv")
    system(paste("gsutil cp", file_path, paste0(disease,"_scores_nostd.csv")))
    }

#standardize scores and  save into local folder

for (disease in diseases) {
    local_path <- paste0(disease, "_scores_nostd.csv")
    standardized_local_path <- paste0(disease, "_scores.csv")

    # Read the CSV file
    data <- read.csv(local_path)
  
    # Standardize the 'score' column
    data$score <- scale(data$score)
    
    print(head(data))
    print(c(disease, mean(data$score), sd(data$score)))
    
    # Save the standardized file locally
    write.csv(data, standardized_local_path, row.names = FALSE)
}

#export to cloud

for (disease in diseases) {
    standardized_local_path <- paste0(disease, "_scores.csv")
    standardized_remote_path <- paste0(bucket, "/scores/", disease, "_scores.csv")
    system(paste("gsutil cp", standardized_local_path, standardized_remote_path))
    }

for(disease in diseases){
    
    prs <- read.csv(paste0(disease,'_scores.csv'))
    prs <- merge(prs, data_prs, by.x="s", by.y="person_id", all.x=TRUE)
    
    # Define subgroups
    subgroups <- list(
        full_sample = prs,
        hispanic = subset(prs, rande == "Hispanic"),
        non_hispanic_white = subset(prs, rande == "Non-Hispanic White"),
        non_hispanic_black = subset(prs, rande == "Non-Hispanic Black"),
        non_hispanic_other = subset(prs, rande == "Other")
    )

    for (subgroup in names(subgroups)) {
        
        subgroup_data <- subgroups[[subgroup]]

        roc_curve <- pROC::roc(subgroup_data[[disease]], subgroup_data$score)
        auroc <- pROC::auc(roc_curve)
        roc_data <- data.frame(
            tpr = roc_curve$sensitivities,
            fpr = 1 - roc_curve$specificities
        )
        
        race = case_when(
            subgroup == "full_sample" ~ "Full sample",
            subgroup == "hispanic" ~ "Hispanic",
            subgroup == "non_hispanic_white" ~ "Non-Hispanic White",
            subgroup == "non_hispanic_black" ~ "Non-Hispanic Black",
            subgroup == "non_hispanic_other" ~ "Other"
          )

        # Display the first few rows of the data frame to verify
        head(df)
        plot <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
            geom_line() +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
            labs(title = paste(race), x = "False Positive Rate", y = "True Positive Rate") +
            theme_classic() +
            theme(text=element_text(size=20))

        # Nagelkerke pseudo R2
        model <- glm(as.formula(paste(disease, "~ score")), data = subgroup_data, family = binomial)
        nagelkerke_r2 <- NagelkerkeR2(model)$R2

        print(paste0(disease, "-", subgroup))
        print(auroc)
        print(paste("Nagelkerke's R-squared:",nagelkerke_r2))
        print(plot)
        
    }
}


