
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

aggregate_squarefoot <- function(squarefoot, vals, by){
  
  # browser()
  squarefoot_vals <- squarefoot |> 
    ungroup() |> 
    (\(x)x[,vals])()
  
  by_squarefoot <- aggregate(squarefoot_vals, by, FUN = mean,na.rm = TRUE)
  
  by_squarefoot$n <- aggregate(squarefoot[,1], by, FUN = length) |>
    st_drop_geometry() |> 
    (\(x) x[,1])()
  
  cbind(by_squarefoot, st_drop_geometry(by))
}



################################################################################################################
## Import Data
################################################################################################################


## ↳ Generic Geodata
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





## ↳ squarefoot Data
################################################################################################################

# output
gpkg_path_squarefoot <- "appdata/vectors_squarefoot.gpkg"


squarefoot <- read_csv("one-off scripts/Squarefoot_data_long.csv")
#resurvey$dataset_id <- as.numeric(factor(resurvey$Time)) ########### 1 = delta, 3 = resurvey, 2 = historic


id_cols <- c("PAG")
independent_vars <- c("Time","Precision")#, "Altitude")
# dependent_vars <- c("Species_richness",
#                    "Phylogenetic_diversity",
#                    "Functional_diversity",
#                    "Funct_div_spec_leaf_area",
#                    "Funct_div_seed_mass",
#                    "Funct_div_height",
#                    "Temperature",
#                    "Nutrient",
#                    "Reaction",
#                    "Moisture",
#                    "Light",
#                    "Moving_tolerance",
#                    "EIV_Hemeroby",
#                    "Cover_Poaceae",
#                    "Cover_Forb",
#                    "Cover_Cyp_Junc",
#                    "CSR_Stress_tolerance",
#                    "CSR_Disturbance_tolerance",
#                    "CSR_Competitive_ability")

dependent_vars <- c("Altitude",
                    "Anzahl_Arten",
                    "Phylogenetische_Diversität",
                    "Funktionale_Diversität",
                    "Funktionale_Diversität_spezifische_Blattfläche",
                    "Funktionale_Diversität_Samengewicht",
                    "Funktionale_Diversität_Höhe",
                    "Temperatur",
                    "Nährstoff",
                    "Reaktion",
                    "Feuchtigkeit",
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




encode_clean_names <- function(df){
  nm <- names(df)
  nm <- tolower(nm)                        # lowercase
  #nm <- gsub("[^a-z0-9]", "_", nm)        # replace non-alphanumeric with _
  nm <- gsub("_+", "_", nm)                # collapse multiple underscores
  nm <- gsub("^_|_$", "", nm)            # remove leading/trailing underscore
  
  umlaute <- c("ä" = "a0e", "ö" = "o0e", "ü" = "u0e")
  nm <- str_replace_all(nm, umlaute)   # Encode special characters/umlaute
  
  names(df) <- nm

  return(df)
  
}




# # Replace underscores with spaces and capitalize
# str_vec <- str_vec |>
#   str_replace_all("_", " ") |>
#   str_to_title()
# 
# # Optional: decode placeholders back to original umlauts
# decode_umlaut <- function(vec, map) {
#   for (char in names(map)) {
#     vec <- str_replace_all(vec, fixed(map[char]), char)
#   }
#   vec
# }
# 
# decoded <-  str_vec |>
#   str_replace_all("a0e", "ä") |>
#   str_replace_all("o0e", "ö") |>
#   str_replace_all("u0e", "ü") |>
#   str_replace_all("A0e", "Ä") |>
#   str_replace_all("O0e", "Ö") |>
#   str_replace_all("U0e", "Ü") |>
#   str_to_title()
# str_vec    # Cleaned with title case and placeholders
# decoded    # Original umlauts restored




squarefoot <- squarefoot[,c(id_cols,independent_vars,dependent_vars, coordinate_cols)]  |>
  filter(if_any(matches(coordinate_cols), \(x)!is.na(x))) |>
  st_as_sf(coords = coordinate_cols, crs = 2056) #|>
  #janitor::clean_names()
squarefoot <- encode_clean_names(squarefoot)

cbind(st_drop_geometry(squarefoot),
  st_coordinates(squarefoot)) |>
  write_csv("appdata/squarefoot.csv")


# #see some stats
# squarefoot_df <- data.frame(squarefoot)
# num_cols <- sapply(squarefoot_df, is.numeric)  # only numeric columns
# summary_stats <- data.frame(
#   mean = sapply(squarefoot_df[, num_cols], mean, na.rm = TRUE),
#   min  = sapply(squarefoot_df[, num_cols], min, na.rm = TRUE),
#   max  = sapply(squarefoot_df[, num_cols], max, na.rm = TRUE),
#   median  = sapply(squarefoot_df[, num_cols], median, na.rm = TRUE)
# )
# summary_stats[-(1:3), ]
# 
# squarefoot_df_h <- squarefoot_df[squarefoot_df$time == "historic",]
# squarefoot_df_r <- squarefoot_df[squarefoot_df$time == "resurvey",]
# squarefoot_df_d <- squarefoot_df[squarefoot_df$time == "delta",]
# 
# squarefoot_df_h_mean <- mean(squarefoot_df_h$temperature)
# squarefoot_df_r_mean <- mean(squarefoot_df_r$temperature)
# 
# 
# plot(squarefoot_df_h$pag, squarefoot_df_h$temperature, col="red")
# points(squarefoot_df_r$pag, squarefoot_df_r$temperature, col="blue")
# abline(h=squarefoot_df_h_mean, col='red')
# abline(h=squarefoot_df_r_mean, col='blue')



#threshold <- read_xlsx("tmp_resurvey_2025-01_annual_trends_threshold_definitions.xlsx", "Schwellenwerte")

###############################################################################################################
## Aggregate and write Data
################################################################################################################


## ↳ Prepare Spatial Aggregation Layers
################################################################################################################

hex5 <- st_make_grid(schweiz, 5000, square = FALSE) |>
  st_as_sf() |>
  mutate(hex5 = row_number())

hex10 <- st_make_grid(schweiz, 10000, square = FALSE) |>
  st_as_sf() |>
  mutate(hex10 = row_number())

hex20 <- st_make_grid(schweiz, 20000, square = FALSE) |>
  st_as_sf() |>
  mutate(hex20 = row_number())


hex10_BGR <- hexagonize(hex10, BGR, DERegionNa)

BGR <- BGR |>
  group_by(bgr = DERegionNa) |>
  summarise()


kantone <- kantone |>
  group_by(kantone = NAME) |>
  summarise()



## Aggregate Data to Polygon and export to gpkg
################################################################################################################
# vals <- c("species_richness",
#           "phylogenetic_diversity",
#           "functional_diversity",
#           "funct_div_spec_leaf_area",
#           "funct_div_seed_mass",
#           "funct_div_height",
#           "temperature",
#           "nutrient",
#           "reaction",
#           "moisture",
#           "light",
#           "moving_tolerance",
#           "urbanization",
#           "cover_poaceae",
#           "cover_forb",
#           "cover_cyp_junc",
#           "csr_stress_tolerance",
#           "csr_disturbance_tolerance",
#           "csr_competitive_ability")
vals <- c("altitude",
          "anzahl_arten",
          "phylogenetische_diversita0et",
          "funktionale_diversita0et",
          "funktionale_diversita0et_spezifische_blattfla0eche",
          "funktionale_diversita0et_samengewicht",
          "funktionale_diversita0et_ho0ehe",
          "temperatur",
          "na0ehrstoff",
          "reaktion",
          "feuchtigkeit",
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

historisch <- squarefoot[squarefoot$time == "historic", ]
resurvey <- squarefoot[squarefoot$time == "resurvey", ]
delta <- squarefoot[squarefoot$time == "delta", ]


aggregate_write_aggregations(historisch, vals, hex10, hex20, BGR, kantone, gpkg_path_squarefoot)
aggregate_write_aggregations(resurvey, vals, hex10, hex20, BGR, kantone, gpkg_path_squarefoot)
aggregate_write_aggregations(delta, vals, hex10, hex20, BGR, kantone, gpkg_path_squarefoot)


#######################################################################################################################################
#check data
csv_path_sqft <- "appdata/squarefoot.csv"
gpkg_path_sqft <- "appdata/vectors_squarefoot.gpkg"



#' Load all layers from a GeoPackage file
#' @param file Path to the GeoPackage file
#' @param exception Layer names to exclude
#' @return A list of sf objects
load_geodata <- function(file = DATA_CONFIG$gpkg_path, exception = NA) {
  layer_names <- st_layers(file)$name
  layer_names <- layer_names[!(layer_names %in% exception)]
  sapply(layer_names, \(x)st_read(file, x), simplify = FALSE)
}

#' Load all layers from a GeoPackage file
#' @param file Path to the GeoPackage file
#' @param exception Layer names to exclude
#' @return A list of sf objects
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

names(data_geo_sqft$hex10_historisch)

plot(data_measurements_sqft$altitude, data_measurements_sqft$anzahl_arten, xlab="Altitude", ylab="Anzahl Arten")
plot(data_measurements_sqft$altitude, data_measurements_sqft$phylogenetische_diversitat, xlab="Altitude", ylab="Anzahl Arten")








###############################################################################
  
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
i <- "historisch"
geodata$i
geodata[[i]]
geodata[["historisch"]]
#######################################################################################################################################

