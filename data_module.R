# Data Module Functions

# load thresholds for the color coding 
#library(readxl)
threshold_delta <- read_xlsx("appdata/threshold_definitions_delta_de.xlsx", sheet="Schwellenwerte")
threshold_res_hist <- read_xlsx("appdata/threshold_definitions_res_hist_de.xlsx", sheet="Schwellenwerte")

#' select the dataset based on inputs: time aspect and aggregation
#' @param list_of_datasets all the loaded datasets
#' @param selected_aggregation the input aggregation
#' @param selected_time the input time aspect
#' @return a single subsetted dataset with the variables and geometric information
select_dataset <-function(list_of_datasets,
                          selected_aggregation,
                          selected_time,
                          sep = "_") { 
  layer_name <- paste(selected_aggregation, selected_time, sep = sep)
  return(na.omit(list_of_datasets[[layer_name]]))
}

#' Load all layers from a GeoPackage file
#' @param file Path to the GeoPackage file
#' @param exception Layer names to exclude
#' @return A list of sf objects
load_geodata <- function(file = DATA_CONFIG$gpkg_path, exception = NA) {
  layer_names <- st_layers(file)$name
  layer_names <- layer_names[!(layer_names %in% exception)]
  data_geo_sqft <- sapply(layer_names, \(x)st_read(file, x), simplify = FALSE)
  return(data_geo_sqft)
}

#' Load all layers from a GeoPackage file - the data gets the time aspect as separate, additional level
#' @param file Path to the GeoPackage file
#' @param exception Layer names to exclude
#' @return 3 lists of sf objects - historic, resurvey and delta
load_geodata_2 <- function(file = DATA_CONFIG$gpkg_path, exception = NA) {
  layer_names <- st_layers(file)$name
  layer_names <- layer_names[!(layer_names %in% exception)]
  data_geo_sqft <- sapply(layer_names, \(x)st_read(file, x), simplify = FALSE)
  
  historisch_list <- data_geo_sqft[grep("historisch$", names(data_geo_sqft))]
  resurvey_list <- data_geo_sqft[grep("resurvey$", names(data_geo_sqft))]
  delta_list <- data_geo_sqft[grep("delta$", names(data_geo_sqft))]
  
  old_names <- names(historisch_list)
  new_names <- sub("_historisch$", "", old_names) 
  names(historisch_list) <- new_names
  
  old_names <- names(resurvey_list)
  new_names <- sub("_resurvey$", "", old_names) 
  names(resurvey_list) <- new_names
  
  old_names <- names(delta_list)
  new_names <- sub("_delta$", "", old_names) 
  names(delta_list) <- new_names
  
  geodata <- list(historisch = historisch_list, resurvey = resurvey_list, delta = delta_list)
  return(geodata)
}

#' Load dataset information
#' @return A data frame with dataset information
load_dataset_info <- function() {
  read_csv(DATA_CONFIG$csv_path)
}

#' Filter data based on user inputs
#' @param data The data to filter
#' @param time_aspect Selected dataset time aspect
#' @return Filtered data
filter_data <- function(data, time_aspect) { 
  data |>
    filter(time %in% time_aspect)
}

#' Get column values for visualization
#' @param data The data frame
#' @param column_name The name of the column to get values from
#' @return The column values
get_column_values <- function(data, column_name) {
  if (column_name == "n") {
    return(data$n)
  }
  return(data[[column_name]])
} 

# read_all_layers <- function(file, exception = NA) {
#   layer_names <- st_layers(file)$name
#   layer_names <- layer_names[!(layer_names %in% exception)]
#   sapply(layer_names, \(x)st_read(file, x), simplify = FALSE)
# }