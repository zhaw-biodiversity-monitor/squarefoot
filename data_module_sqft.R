# Data Module Functions


#threshold <- read_csv("appdata/thresholds.csv")
library(readxl) ##########################################
threshold <- read_xlsx("tmp_resurvey_2025-01_annual_trends_threshold_definitions.xlsx", sheet="Schwellenwerte")##########################################


#' Load all layers from a GeoPackage file
#' @param file Path to the GeoPackage file
#' @param exception Layer names to exclude
#' @return A list of sf objects
load_geodata <- function(file = DATA_CONFIG$gpkg_path, exception = NA) {
  layer_names <- st_layers(file)$name
  layer_names <- layer_names[!(layer_names %in% exception)]
  sapply(layer_names, \(x)st_read(file, x), simplify = FALSE)
}

#' Load dataset information
#' @return A data frame with dataset information
load_dataset_info <- function() {
  read_csv(DATA_CONFIG$csv_path)
}

#' Filter data based on user inputs
#' @param data The data to filter
#' @param dataset_ids Selected dataset IDs
#' @param habitat_groups Selected habitat groups
#' @param area_range Range of plot sizes
#' @return Filtered data
filter_data <- function(data, dataset_ids, habitat_groups, area_range) {
  data |>
    filter(dataset_id %in% dataset_ids) |>
    filter(lebensraumgruppe %in% habitat_groups) |>
    filter(flache_m2 > area_range[1], flache_m2 < area_range[2])
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

