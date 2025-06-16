# load libraries ####
#install.packages("renv")
# Function to load libraries from CRAN
install_and_load_library <- function(lib_names) {
  missing_libs <- lib_names[!sapply(lib_names, requireNamespace, quietly = TRUE)]
  
  if (length(missing_libs) > 0) {
    suppressMessages(suppressWarnings(renv::install(missing_libs, dependencies = TRUE)))
  }
  
  # Load all libraries
  for (lib in lib_names) {
    suppressMessages(suppressWarnings(library(lib, character.only = TRUE)))
  }
}


libraries <- c("readr","tidyverse","writexl","ggrepel","knitr",
                "kableExtra","openxlsx", "ggfortify","ggpubr","rstatix", "rrcov",
               "ggalluvial", "networkD3", "reactable", "arsenal", 
               "skimr", "funModeling", "inspectdf", "DataExplorer", "PerformanceAnalytics","corrplot", "flextable", "rmarkdown","patchwork","ggthemes", "ggpubr", "fastDummies",
               "naniar", "mice", "VIM", "dlookr", "SmartEDA", "randomForest", "missForest", "sampling")

install_and_load_library(libraries)
# Snapshot your environment after installing
renv::snapshot(prompt = TRUE,  exclude = c("input_data/A4/Image"))
options(renv.config.dependencies.limit = Inf)

rm("install_and_load_library", "libraries" )
