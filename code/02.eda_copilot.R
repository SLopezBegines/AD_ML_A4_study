# Functions for Exploratory Data Analysis

# ## Create a function to extract the data dictionary
extract_data_dictionary <- function(data, data_dict) {
  # Get the field names from the data dictionary
  field_names <- unique(data_dict)
  
  # Initialize a list to store the extracted data
  extracted_data <- list()
  
  # Loop through each field name and extract the corresponding data
  for (field in field_names) {
    if (field %in% names(data)) {
      extracted_data[[field]] <- data[[field]]
    } else {
      warning(paste("Field", field, "not found in the data."))
    }
  }
  
  return(extracted_data)
}
# ## Create a function to summarize the data
summarize_data <- function(data) {
  summary_stats <- data %>%
    select(where(is.numeric)) %>%  # Select only numeric columns for summary statistics
    summarise(across(everything(), list(
      mean = ~ mean(.x, na.rm = TRUE),
      median = ~ median(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE),
      min = ~ min(.x, na.rm = TRUE),
      max = ~ max(.x, na.rm = TRUE),
      n_missing = ~ sum(is.na(.x))
    )))
  print(summary_stats)
  return(summary_stats)
}
# ## Create a function to visualize the data
visualize_data <- function(data) {
  plots <- list()
  
  # Histogram for numeric variables
  numeric_vars <- sapply(data, is.numeric)
  for (var in names(data)[numeric_vars]) {
    p <- ggplot(data, aes(x = var)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
      theme_minimal()
    print(p)
    plots[[var]] <- p
  }
  
  # Bar plot for categorical variables
  categorical_vars <- sapply(data, is.factor)
  for (var in names(data)[categorical_vars]) {
    p <- ggplot(data, aes(x = var)) +
      geom_bar(fill = "blue", color = "black") +
      labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
      theme_minimal()
    print(p)
    plots[[var]] <- p
  }
  
  return(plots)
}
# ## Create a function to perform exploratory data analysis
perform_eda <- function(data, data_dict) {
  # Extract data dictionary
  extracted_data <- extract_data_dictionary(data, data_dict)
  
  # Summarize data
  summary_stats <- summarize_data(data)
  
  # Visualize data
  plots <- visualize_data(data)
  
  return(list(
    extracted_data = extracted_data,
    summary_stats = summary_stats,
    plots = plots
  ))
}



