
setwd("C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/") #yaelh und hedd 

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

BGR <- read_sf("biogreg/BiogeographischeRegionen/N2020_Revision_BiogeoRegion.shp") |>
  st_zm()


kantone <- read_sf("swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp") |>
  st_zm() |>
  select(NAME, KANTONSNUM) |>
  st_set_crs(2056)

schweiz <- read_sf("swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp") |>
  st_zm() |>
  filter(NAME != "Liechtenstein") |>
  st_union() |>
  st_set_crs(2056)





## ↳ Resurvey Data
################################################################################################################

# output
gpkg_path_squarefoot <- "vectors_squarefoot.gpkg"


squarefoot <- read_csv("Squarefoot_data_long.csv")
#resurvey$dataset_id <- as.numeric(factor(resurvey$Time)) ########### 1 = delta, 3 = resurvey, 2 = historic 
#resurvey$Lebensraumgruppe <- resurvey$Time 

id_cols <- c("PAG")
independent_vars <- c("Time","Precision", "Altitude")#, "Lebensraumgruppe") #dataset_id
dependent_vars <- c("Species_richness",
                   "Phylogenetic_diversity",
                   "Functional_diversity",
                   "Funct_div_spec_leaf_area",
                   "Funct_div_seed_mass",
                   "Funct_div_height",
                   "Temperature",
                   "Nutrient",
                   "Reaction",
                   "Moisture",
                   "Light",
                   "Moving_tolerance",
                   "Urbanization",
                   "Cover_Poaceae",
                   "Cover_Forb",
                   "Cover_Cyp_Junc",
                   "CSR_Stress_tolerance",
                   "CSR_Disturbance_tolerance",
                   "CSR_Competitive_ability")
coordinate_cols <- c("Center_x_coordinate", "Center_y_coordinate") #2056? or 4326?




# Quality Checks
stopifnot(id_cols %in% colnames(squarefoot) |> all())
stopifnot(independent_vars %in% colnames(squarefoot) |> all())
stopifnot(dependent_vars %in% colnames(squarefoot) |> all())
stopifnot(coordinate_cols %in% colnames(squarefoot) |> all())




# more Quality checks?







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




## ↳ Resurvey: Aggregate Data to Polygon and export to gpkg
################################################################################################################
squarefoot <- squarefoot[,c(id_cols,independent_vars,dependent_vars, coordinate_cols)]  |>
  filter(if_any(matches(coordinate_cols), \(x)!is.na(x))) |>
  st_as_sf(coords = coordinate_cols, crs = 2056) |>
  janitor::clean_names()


# 
# names(resurvey)
# resurvey_sf <- resurvey |> 
#   imap_dfr(\(x,y){
#     st_as_sf(x, coords = c("Center_x_coordinate","Center_y_coordinate"), crs = as.integer(y)) |> 
#       st_transform(2056)
#   })
# 
# 
# resurvey_clean <- resurvey_sf[,c(id_cols, independen_vars, dependen_vars)] |> 
#   janitor::clean_names()
# 
# resurvey_clean <- cbind(resurvey_clean, st_coordinates(resurvey_clean)) |> 
#   st_drop_geometry()
# 
# # making all 
# # vals <- colnames(resurvey_clean) |> tail(-8) |> head(-1)
# vals <- c("artenzahl","relative_artenzahl","shannon_index", "shannon_evenness", "temperaturzahl", "kontinentalitatszahl", "lichtzahl", "feuchtezahl",  "reaktionszahl", "nahrstoffzahl", "humuszahl", "konkurrenzzahl", "ruderalzahl", "stresszahl", "mahdvertraglichkeit")
# 
# resurvey_clean <- resurvey_clean |> 
#   arrange(unique_plot_id, jahr)
# 
# 
# # Quality Check: Are there plots for which we only have one survey?
# # https://github.com/zhaw-biodiversity-monitor/zhaw-biodiversity-monitor.github.io/issues/11
# resurvey_clean |> 
#   count(unique_plot_id) |> 
#   filter(n == 1) |> 
#   pull(unique_plot_id) |>
#   unique() |> 
#   cat(sep = "\n")
# 
# # Deng says: remove them for now (see issue above)
# resurvey_clean <- resurvey_clean |> 
#   add_count(unique_plot_id,name = "n_plots") |> 
#   filter(n_plots >= 2) |> 
#   select(-n_plots)
# 
# 
# 
# 
# # https://github.com/zhaw-biodiversity-monitor/zhaw-biodiversity-monitor.github.io/issues/22
# resurvey_clean |> 
#   group_by(unique_plot_id, X, Y) |> 
#   summarise() |> 
#   group_by(unique_plot_id) |>
#   add_count() |> 
#   filter(n > 1) |> 
#   select(-n) |> 
#   mutate(i = row_number()) |> 
#   pivot_wider(names_from = i, values_from = c(X, Y)) |> 
#   ungroup() |> 
#   mutate(
#     dist = sqrt((X_1-X_2)^2 + (Y_1-Y_2)^2)
#   ) |> 
#   arrange(desc(dist)) |> 
#   mutate(i = row_number(), .before = 1) |>
#   knitr::kable()
# 
# 
# # Just take the mean of X/Y for now
# resurvey_clean <- resurvey_clean |> 
#   group_by(unique_plot_id) |> 
#   mutate(X = mean(X), Y = mean(Y))
# 
# 
# # Quality Check: are there plots with multiple flache_m2 etc? (no)
# resurvey_clean |> 
#   group_by(unique_plot_id, dataset_id, plot_id, flache_m2, jahr) |> 
#   summarise() |> 
#   group_by(unique_plot_id) |>
#   add_count() |> 
#   filter(n > 1)
# 
# resurvey_two_years <- resurvey_clean |> 
#   group_by(unique_plot_id, dataset_id, plot_id, flache_m2, lebensraumgruppe, X, Y) |>
#   mutate(
#     i = row_number(),
#     i_max = max(i)
#   ) |> 
#   filter(i == 1 | i == i_max)
# 
# 
# # Quality Check: Do we have two years for each plot / group?
# stopifnot((n_groups(resurvey_two_years)*2) == nrow(resurvey_two_years))
# 
# 
# resurvey_two_years
# 
# # This should be the new value for shannon_index:
# (2.88-3.16)/(2020-1980)
# 
# resurvey_smry <- resurvey_two_years |>
#   summarise(across(matches(vals), \(x)(x[2]-x[1])/(jahr[2]-jahr[1])))|> 
#   st_as_sf(coords = c("X", "Y"), crs = 2056)






threshold <- read_xlsx("tmp_resurvey_2025-01_annual_trends_threshold_definitions.xlsx", "Schwellenwerte")


# aggregate_squarefoot <- function(squarefoot, vals, by){
#   
#   # browser()
#   squarefoot_vals <- squarefoot |> 
#     ungroup() |> 
#     (\(x)x[,vals])()
#   
#   by_squarefoot <- aggregate(squarefoot_vals, by, FUN = mean,na.rm = TRUE)
#   
#   
#   by_squarefoot$n <- aggregate(squarefoot[,1], by, FUN = length) |>
#     st_drop_geometry() |> 
#     (\(x) x[,1])()
#   
#   cbind(by_squarefoot, st_drop_geometry(by))
# }




#vals <- c("artenzahl","relative_artenzahl","shannon_index", "shannon_evenness", "temperaturzahl", "kontinentalitatszahl", "lichtzahl", "feuchtezahl",  "reaktionszahl", "nahrstoffzahl", "humuszahl", "konkurrenzzahl", "ruderalzahl", "stresszahl", "mahdvertraglichkeit")
vals <- c("species_richness",
                    "phylogenetic_diversity",
                    "functional_diversity",
                    "funct_div_spec_leaf_area",
                    "funct_div_seed_mass",
                    "funct_div_height",
                    "temperature",
                    "nutrient",
                    "reaction",
                    "moisture",
                    "light",
                    "moving_tolerance",
                    "urbanization",
                    "cover_poaceae",
                    "cover_forb",
                    "cover_cyp_junc",
                    "csr_stress_tolerance",
                    "csr_disturbance_tolerance",
                    "csr_competitive_ability")

hex10_squarefoot <- aggregate_squarefoot(squarefoot, vals, hex10) |>
  st_transform(4326) |> 
  filter(!is.na(n))


# hex10_resurvey[,!(colnames(hex10_resurvey) %in% threshold$Parameter)]
# 
# threshold_long <- threshold |> 
#   pivot_longer(-Parameter)
# 
# map(threshold$Parameter, \(x){
#   browser()
#   
#   thr <- pivot_longer(threshold[threshold$Parameter == x, ], -Parameter)
#   cut(hex10_resurvey[,x, drop = TRUE], c(-Inf, thr$value))
# })




hex20_squarefoot <- aggregate_squarefoot(squarefoot, vals, hex20) |> 
  st_transform(4326) |> 
  filter(!is.na(n))

bgr_squarefoot <- aggregate_squarefoot(squarefoot, vals, BGR) |> 
  st_transform(4326) |> 
  filter(!is.na(n))

kantone_squarefoot <- aggregate_squarefoot(squarefoot, vals, kantone) |> 
  st_transform(4326) |> 
  filter(!is.na(n))



if(file.exists(gpkg_path_squarefoot))file.remove(gpkg_path_squarefoot)
write_sf(hex10_squarefoot, gpkg_path_squarefoot, "hex10", delete_layer = TRUE)
write_sf(hex20_squarefoot, gpkg_path_squarefoot, "hex20", delete_layer = TRUE)
write_sf(bgr_squarefoot, gpkg_path_squarefoot, "bgr", delete_layer = TRUE)
write_sf(kantone_squarefoot, gpkg_path_squarefoot, "kantone", delete_layer = TRUE)

squarefoot |> 
  st_transform(4326) |>
  write_sf(gpkg_path_squarefoot, "punkte", delete_layer = TRUE)


layers <- tibble(layer_name = st_layers(gpkg_path_squarefoot)$name)

cbind(
  st_drop_geometry(squarefoot),
  st_coordinates(squarefoot)
) |>
  write_csv("squarefoot.csv")






#######################################################################################################################################
#check data

csv_path_res <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/grassland-data-main/appdata/resurvey.csv"
gpkg_path_res <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/grassland-data-main/appdata/vectors_resurvey.gpkg"
gpkg_path_tot <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/grassland-data-main/appdata/vectors.gpkg"

csv_path_sqft <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/squarefoot.csv"
gpkg_path_sqft <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/vectors_squarefoot.gpkg"


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

layers_res <- st_layers(gpkg_path_res)$name
layers_tot <- st_layers(gpkg_path_tot)$name
layers_sqft <- st_layers(gpkg_path_sqft)$name


data_geo_res <- load_geodata(file = gpkg_path_res, exception = NA)
data_measurements_res <- read_csv(csv_path_res)
data_geo_tot <- load_geodata(file = gpkg_path_tot, exception = NA)

data_geo_sqft <- load_geodata(file = gpkg_path_sqft, exception = NA)
data_measurements_sqft <- read_csv(csv_path_sqft)


#######################################################################################################################################
