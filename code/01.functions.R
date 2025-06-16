

# Creat directories to store data
create_directories <- function(base_path) {
  # Helper function to create a directory if it does not exist
  create_dir_if_not_exists <- function(path) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE,showWarnings = FALSE)
      cat("Directory created:", path, "\n")
    } else {
      cat("Directory already exists:", path, "\n")
    }
  }
  
  # Create the base directory and its subdirectories
  create_dir_if_not_exists(base_path)
  create_dir_if_not_exists(paste0(base_path, "tables"))
  create_dir_if_not_exists(paste0(base_path, "figures"))
  create_dir_if_not_exists(paste0(base_path, "RData"))
}



#Save Plots

# Función para guardar gráficos en TIFF y PDF
save_plot <- function(plotname, 
                      plot, 
                      width = 16, 
                      height = 6, 
                      dpi = 300,
                      tiff_extension = ".tiff",
                      pdf_extension = ".pdf") {
  # Verificar si plot es un objeto válido de ggplot
  if (!inherits(plot, "ggplot")) {
    warning(paste("The object", plotname, "is not a valid pheatmap object. Skipping saving."))
    return(NULL)
  }
  
  # Construcción del nombre de archivo con contador
  filename <- paste0(output_path, "figures/", sprintf("%03d", image_number), "_", plotname)
  
  # Guardar en TIFF y PDF
  tryCatch({
    ggsave(paste0(filename, tiff_extension), plot, width = width, height = height, units = "cm", dpi= dpi)
    ggsave(paste0(filename, pdf_extension), plot, width = width, height = height, units = "cm")
    
    # Incrementar el contador global de imágenes
    image_number <<- image_number + 1
  }, error = function(e) {
    warning(paste("No se pudo guardar la imagen:", filename, "Error:", e$message))
  })
rm(p)  
}



# Función para guardar gráficos base R en TIFF y PDF
save_base_plot <- function(plotname, plot_function, width = 10, height = 10, res = 300, show_plot = TRUE) {
  filename <- paste0(output_path, "figures/", sprintf("%03d", image_number), "_", plotname)
  
  tryCatch({
    # Guardar TIFF
    tiff(file = paste0(filename, ".tiff"), width = width, height = height, units = "in", res = res)
    plot_function()
    dev.off()
    
    # Guardar PDF
    pdf(file = paste0(filename, ".pdf"), width = width, height = height)
    plot_function()
    dev.off()
    
    # Mostrar en pantalla si se desea
    if (show_plot) {
      plot_function()
    }
    
    image_number <<- image_number + 1
  }, error = function(e) {
    warning(paste("No se pudo guardar la imagen base R:", filename, "Error:", e$message))
  })
}



## reactViewTable:
### librería: reactable
### objetivo: ver los datos de forma interactiva y dinámica en HTML
reactViewTable <- function(data) {
  reactable::reactable(
    data,
    bordered = TRUE,
    borderless = FALSE,
    highlight = TRUE,
    outlined = TRUE,
    resizable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    showSortIcon = TRUE,
    showSortable = TRUE,
    showPageSizeOptions = TRUE,
    defaultPageSize = 15,
    pageSizeOptions = c(5, 10, 20, 50, 100),
    width = "100%",
    theme = reactable::reactableTheme(
      headerStyle = list(
        backgroundColor = "#095540",
        color = "white",
        fontWeight = "bold"
      ),
      rowStyle = list(
        backgroundColor = "#efeee0"
      ),
      borderColor = "#ccc",
      stripedColor = "#f5f5f5",
      highlightColor = "#e0f7e9"
    ),
    defaultColDef = reactable::colDef(
      format = reactable::colFormat(digits = 5),
      align = "center"
    )
  )
}
# Function to create a summary table with arsenal
create_summary_table <- function(data, group_var = NULL) {
  if (is.null(group_var)) {
    summary_table <- arsenal::tableby(~ ., data = data)
  } else {
    summary_table <- arsenal::tableby(as.formula(paste("~", group_var)), data = data)
  }
  
  # Convert to a data frame for better display
  summary_df <- as.data.frame(summary_table)
  
  # Return the summary table
  return(summary_df)
}

# Function to load and clean data from CSV files
load_and_clean_data <- function(file_path) {
  # Read the CSV file
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Convert character columns to factors
  df <- mutate(df, across(where(is.character), as.factor))
  
  # Remove columns with only one level factor
  one_level_cols <- names(df)[sapply(df, function(col) is.factor(col) && nlevels(col) == 1)]
  if (length(one_level_cols) > 0) {
    df <- select(df, -all_of(one_level_cols))
    message(paste("Removed columns with only one level factor:", paste(one_level_cols, collapse = ", ")))
  }
  
  return(df)
}



#' @title Aplicar clases de columnas desde una tabla resumen
#' @description
#' Esta función aplica tipos de datos (clases) a las columnas de un dataframe según lo indicado en una tabla de estructura (`structure_summary`).
#' Muy útil para armonizar tipos antes de análisis o modelado.
#'
#' @param df Un dataframe cuyas columnas se desean tipar (por ejemplo, `cross_sectional_df` o `longitudinal_df`).
#' @param structure_summary Un dataframe con dos columnas:
#'   - `column`: nombres de columnas en `df`
#'   - `class`: clase deseada para cada columna (`character`, `numeric`, `factor`, `integer`, `logical`, etc.)
#'
#' @return El dataframe original `df` con sus columnas convertidas a las clases especificadas en `structure_summary`.
#' 
#' @examples
#' structure_summary <- tibble::tibble(
#'   column = c("AGE", "SEX", "BID"),
#'   class = c("numeric", "factor", "character")
#' )
#' df_typed <- apply_column_classes(df, structure_summary)
#'
#' @export
apply_column_classes <- function(df, structure_summary) {
  # Filtrar solo columnas que estén presentes en el df
  applicable <- structure_summary %>%
    filter(column %in% colnames(df))
  
  # Aplicar transformación tipo a tipo
  for (i in seq_len(nrow(applicable))) {
    col <- applicable$column[i]
    class_type <- applicable$class[i]
    
    df[[col]] <- switch(class_type,
                        character = as.character(df[[col]]),
                        numeric = as.numeric(df[[col]]),
                        integer = as.integer(df[[col]]),
                        factor = as.factor(df[[col]]),
                        logical = as.logical(df[[col]]),
                        df[[col]])  # Si no está especificado, dejar igual
  }
  return(df)
}
