
#setwd("C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/") #yaelh und hedd 

################################################################################################################
## Load Libraries and construct funtions
################################################################################################################

library(tidyverse)
library(sf)
library(readxl)
library(glue)

read_all_layers <- function(file) {
  sapply(st_layers(file)$name, \(x)st_read(file, x), simplify = FALSE)
}

delete_all_layers <- function(file) {
  sapply(st_layers(file)$name, \(x)st_delete(file, x), simplify = FALSE)
}


hexagonize <- function(hex, to_be_hexagonized, ..., .na_omit = TRUE, .do_union = TRUE) {
  joined <- st_join(hex, select(to_be_hexagonized, ...), largest = TRUE)
  if (.na_omit) joined <- na.omit(joined)
  if (.do_union) {
    joined <- joined %>%
      group_by(...) %>%
      summarise()
  }
  return(joined)
}

 
#' also get all characters lowercase, get rid of duplicate underscores or overhanging ones 
#' @param sqarefoot a dataframe 
#' @param vals a list of columns to be aggregated
#' @param by by what it should be aggregated - hexagonized grids
#' @return a dataframe with renamed columns
aggregate_squarefoot <- function(squarefoot, vals, by){
  squarefoot_vals <- squarefoot |> 
    ungroup() |> 
    (\(x)x[,vals])()
  
  by_squarefoot <- aggregate(squarefoot_vals, by, FUN = mean,na.rm = TRUE)
  
  by_squarefoot$n <- aggregate(squarefoot[,1], by, FUN = length) |>
    st_drop_geometry() |> 
    (\(x) x[,1])()
  
  cbind(by_squarefoot, st_drop_geometry(by))
}

#' encode umlaute to decode them later to keep them in the final visual without having them in the code
#' also get all characters lowercase, get rid of duplicate underscores or overhanging ones 
#' @param df a dataframe 
#' @return a dataframe with renamed columns
encode_clean_names <- function(df){
  nm <- names(df)
  nm <- tolower(nm)                        # lowercase
  nm <- gsub("_+", "_", nm)                # collapse multiple underscores
  nm <- gsub("^_|_$", "", nm)              # remove leading/trailing underscore
  
  umlaute <- c("ä" = "a0e", "ö" = "o0e", "ü" = "u0e")
  nm <- str_replace_all(nm, umlaute)   # Encode special characters/umlaute
  
  names(df) <- nm
  
  return(df)
  
}

################################################################################################################
## Import Data
################################################################################################################


## Generic Geodata
################################################################################################################

BGR <- read_sf("appdata/biogreg/BiogeographischeRegionen/N2020_Revision_BiogeoRegion.shp") |>
  st_zm()


kantone <- read_sf("appdata/swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp") |>
  st_zm() |>
  select(NAME, KANTONSNUM) |>
  st_set_crs(2056)

schweiz <- read_sf("appdata/swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp") |>
  st_zm() |>
  filter(NAME != "Liechtenstein") |>
  st_union() |>
  st_set_crs(2056)





## squarefoot Data
################################################################################################################

# output
gpkg_path_squarefoot <- "appdata/vectors_squarefoot.gpkg"

# data
squarefoot <- read_csv("one-off scripts/Squarefoot_data_long_ger.csv")
#squarefoot$time <- as.numeric(factor(resurvey$Time)) # 1 = delta, 3 = resurvey, 2 = historic


id_cols <- c("PAG")
independent_vars <- c("Time","Precision")#, "Altitude")
dependent_vars <- c("Altitude",
                    "Anzahl_Arten",
                    "Phylogenetische_Diversität",
                    "Funktionale_Diversität",
                    "Funktionale_Diversität_spezifische_Blattfläche",
                    "Funktionale_Diversität_Samengewicht",
                    "Funktionale_Diversität_Höhe",
                    "Temperaturzahl",
                    "Nährstoffzahl",
                    "Reaktionszahl",
                    "Feuchtigkeitszahl",
                    "Licht",
                    "Mahdverträglichkeit",
                    "Hemerobiezahl",
                    "Anteil_Deckung_Poaceae",
                    "Anteil_Deckung_Krautartige",
                    "Anteil_Deckung_Cyperaceae_und_Juncaceae",
                    "Stresszahl",
                    "Ruderalstrategie",
                    "Konkurrenzstrategie")
coordinate_cols <- c("Center_x_coordinate", "Center_y_coordinate") 


# Quality Checks
stopifnot(id_cols %in% colnames(squarefoot) |> all())
stopifnot(independent_vars %in% colnames(squarefoot) |> all())
stopifnot(dependent_vars %in% colnames(squarefoot) |> all())
stopifnot(coordinate_cols %in% colnames(squarefoot) |> all())


squarefoot <- squarefoot[,c(id_cols,independent_vars,dependent_vars, coordinate_cols)]  |>
  filter(if_any(matches(coordinate_cols), \(x)!is.na(x))) |>
  st_as_sf(coords = coordinate_cols, crs = 2056)

# custom clean names for umlaute ä,ö und ü
squarefoot <- encode_clean_names(squarefoot)

cbind(st_drop_geometry(squarefoot),
  st_coordinates(squarefoot)) |>
  write_csv("appdata/squarefoot.csv")


###############################################################################################################
## Aggregate and write Data
################################################################################################################


## Prepare Spatial Aggregation Layers
################################################################################################################

# hex5 <- st_make_grid(schweiz, 5000, square = FALSE) |>
#   st_as_sf() |>
#   mutate(hex5 = row_number())

hex10 <- st_make_grid(schweiz, 10000, square = FALSE) |>
  st_as_sf() |>
  mutate(hex10 = row_number())

hex20 <- st_make_grid(schweiz, 20000, square = FALSE) |>
  st_as_sf() |>
  mutate(hex20 = row_number())

BGR <- BGR |>
  group_by(bgr = DERegionNa) |>
  summarise()

kantone <- kantone |>
  group_by(kantone = NAME) |>
  summarise()


## Aggregate Data to Polygons and export to gpkg
################################################################################################################

vals <- c("altitude",
          "anzahl_arten",
          "phylogenetische_diversita0et",
          "funktionale_diversita0et",
          "funktionale_diversita0et_spezifische_blattfla0eche",
          "funktionale_diversita0et_samengewicht",
          "funktionale_diversita0et_ho0ehe",
          "temperaturzahl",
          "na0ehrstoffzahl",
          "reaktionszahl",
          "feuchtigkeitszahl",
          "licht",
          "mahdvertra0eglichkeit",
          "hemerobiezahl",
          "anteil_deckung_poaceae",
          "anteil_deckung_krautartige",
          "anteil_deckung_cyperaceae_und_juncaceae",
          "stresszahl",
          "ruderalstrategie",
          "konkurrenzstrategie")


aggregate_write_aggregations <- function(squarefoot, vals, hex10, hex20, BGR, kantone, gpkg_path) {

  hex10_squarefoot <- aggregate_squarefoot(squarefoot, vals, hex10) |>
    st_transform(4326) |> 
    filter(!is.na(n))
  
  hex20_squarefoot <- aggregate_squarefoot(squarefoot, vals, hex20) |> 
    st_transform(4326) |> 
    filter(!is.na(n))
  
  bgr_squarefoot <- aggregate_squarefoot(squarefoot, vals, BGR) |> 
    st_transform(4326) |> 
    filter(!is.na(n))
  
  kantone_squarefoot <- aggregate_squarefoot(squarefoot, vals, kantone) |> 
    st_transform(4326) |> 
    filter(!is.na(n))
  
  df_name <- deparse(substitute(squarefoot))
  
  write_sf(hex10_squarefoot, gpkg_path, paste0("hex10_", df_name), delete_layer = TRUE)
  write_sf(hex20_squarefoot, gpkg_path, paste0("hex20_", df_name), delete_layer = TRUE)
  write_sf(bgr_squarefoot, gpkg_path, paste0("bgr_", df_name), delete_layer = TRUE)
  write_sf(kantone_squarefoot, gpkg_path, paste0("kantone_", df_name), delete_layer = TRUE)
  
  squarefoot |> 
    st_transform(4326) |>
    write_sf(gpkg_path_squarefoot, paste0("punkte_", df_name), delete_layer = TRUE)
  
  #layers <- tibble(layer_name = st_layers(gpkg_path_squarefoot)$name)
}

if(file.exists(gpkg_path_squarefoot))file.remove(gpkg_path_squarefoot)

# split the data into the 3 different datasets according to the time column
historisch <- squarefoot[squarefoot$time == "historic", ]
resurvey <- squarefoot[squarefoot$time == "resurvey", ]
delta <- squarefoot[squarefoot$time == "delta", ]

# repeat this aggregation separately for every time dataset (historic, resurvey and delta)
aggregate_write_aggregations(historisch, vals, hex10, hex20, BGR, kantone, gpkg_path_squarefoot)
aggregate_write_aggregations(resurvey, vals, hex10, hex20, BGR, kantone, gpkg_path_squarefoot)
aggregate_write_aggregations(delta, vals, hex10, hex20, BGR, kantone, gpkg_path_squarefoot)


#######################################################################################################################################
#check data by loading it again from the saved gpkg file
csv_path_sqft <- "appdata/squarefoot.csv"
gpkg_path_sqft <- "appdata/vectors_squarefoot.gpkg"


# from the file data_module:

#' Load all layers from a GeoPackage file
#' @param file Path to the GeoPackage file
#' @param exception Layer names to exclude
#' @return A list of sf objects
load_geodata <- function(file = DATA_CONFIG$gpkg_path, exception = NA) {
  layer_names <- st_layers(file)$name
  layer_names <- layer_names[!(layer_names %in% exception)]
  sapply(layer_names, \(x)st_read(file, x), simplify = FALSE)
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

layers_sqft <- st_layers(gpkg_path_sqft)$name

data_geo_sqft <- load_geodata(file = gpkg_path_sqft, exception = NA)
data_measurements_sqft <- read_csv(csv_path_sqft)

data_geo_sqft_2 <- load_geodata_2(file = gpkg_path_sqft, exception = NA)



