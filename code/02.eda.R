# Funtions for Exploratory Data Analysis (EDA)

# Data summary ####
summary_eda <- function(data){
  skim <- skimr::skim(data) 
  status <- funModeling::status(data) 
  profile <- funModeling::profile_num(data)
  
  return(list(
    summary = skim,
    status = status,
    profile = profile
  ))
  
}


# Graphic profile ####

# Remove duplicates ####

# Missing values ####

# Correlations ####
## Numeric variables ####

## Categorical variables. Cramer's V ####

# Imputation of missing values ####

# Outliers ####

# Pearson's correlation ####

# Frequency tables ####

# SMOTE for imbalanced data ####

