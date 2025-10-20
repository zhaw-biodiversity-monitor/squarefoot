
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

aggregate_grass <- function(
    x,
    by,
    weight_col = "design_weight",
    columns_to_weight = c("artenreichtum_gefasspflanzen", "artenreichtum_neophyten", "artenanteil_neophyten", "deckungsanteil_neophyten", "temperaturzahl", "kontinentalitatszahl", "feuchtezahl", "reaktionszahl", "nahrstoffzahl", "strategie_c", "strategie_r", "strategie_s"),
    columns_to_count = "_p_a$", # columns matching this ("matches()") will just be counted
    filter_zero_count = TRUE) {
  ## Checks
  
  # browser()
  stopifnot(all(columns_to_weight %in% names(x)))
  stopifnot(weight_col %in% names(x))
  
  # create a tidy select that I can use with matches()
  columns_to_weight_tidy_sel <- paste(columns_to_weight, collapse = "|")
  
  # multiply all columns that should be weighed with the weight_col
  weighted <- x[c(weight_col, columns_to_weight)] |> # select only the weight col and the columns to weigh
    mutate(
      across(
        matches(columns_to_weight_tidy_sel), # for all columns that should be weighed...
        function(z) {
          z * x[[weight_col]]
        }
      ) # ... multiply by the weigh col
    )
  
  weighted_sum <- cbind(
    weighted,
    st_drop_geometry(select(x, matches("_p_a$")))
  ) |>
    aggregate(by, sum, na.rm = TRUE)
  
  weighted_mean <- weighted_sum |>
    mutate(
      across(
        matches(columns_to_weight_tidy_sel),
        function(z) {
          z / weighted_sum[[weight_col]]
        }
      )
    )
  
  # agg_fun <- aggregate(x_weighted, by, FUN = FUN)
  # agg_fun$n <-
  weighted_mean$n <- aggregate(transmute(x, n = 1), by, FUN = length) |>
    st_drop_geometry() |>
    (\(x) x[, 1])()
  
  weighted_mean <- weighted_mean |>
    mutate(
      across(
        ends_with("_p_a"),
        function(z) {
          z / n
        }
      )
    )
  
  weighted_mean <- cbind(
    weighted_mean,
    st_drop_geometry(by)
  )
  
  
  if (filter_zero_count) {
    weighted_mean <- weighted_mean[weighted_mean$n > 0 & !is.na(weighted_mean$n), ]
  }
  weighted_mean
}

aggregate_resurvey <- function(resurvey, vals, by){
  
  # browser()
  resurvey_vals <- resurvey |> 
    ungroup() |> 
    (\(x)x[,vals])()
  
  by_resurvey <- aggregate(resurvey_vals, by, FUN = mean,na.rm = TRUE)
  
  
  # i <- seq_len(nrow(by_resurvey))
  # by_resurvey_geom <- st_geometry(by_resurvey) |> st_sf()
  # by_resurvey_geom$i <- i
  # st_geometry(by_resurvey_geom) <- "geom"
  # 
  # by_resurvey_vals <- st_drop_geometry(by_resurvey)
  # by_resurvey_vals$i <- i
  # 
  # by_resurvey_lng <- pivot_longer(by_resurvey_vals, -i)
  # 
  # threshold_lng <- pivot_longer(threshold, -Parameter)
  # 
  # left_join(by_resurvey_lng, threshold_lng, by = c(name = "Parameter"), relationship = "many-to-many") |> 
  #   filter()
  
  
  
  
  by_resurvey$n <- aggregate(resurvey[,1], by, FUN = length) |>
    st_drop_geometry() |> 
    (\(x) x[,1])()
  
  cbind(by_resurvey, st_drop_geometry(by))
}

aggregate_infoflora <- function(resurvey, vals, by){
  
  resurvey_vals <- resurvey |> 
    ungroup() |> 
    (\(x)x[,vals])()
  
  by_resurvey <- aggregate(resurvey_vals, by, FUN = mean,na.rm = TRUE)
  by_resurvey$n <- aggregate(resurvey[,1], by, FUN = length) |>
    st_drop_geometry() |> 
    (\(x) x[,1])()
  
  cbind(by_resurvey, st_drop_geometry(by))
}

import_sheet <- function(xlsx, sheet) {
  library(readxl)
  library(janitor)
  library(dplyr)
  read_excel(xlsx, sheet) |>
    janitor::clean_names() |>
    transmute(
      plot_id,
      design_weight,
      x_lv95,
      y_lv95,
      lange,
      breite,
      meereshohe,
      artenreichtum_gefasspflanzen,
      artenreichtum_neophyten,
      artenanteil_neophyten,
      deckungsanteil_neophyten,
      temperaturzahl,
      kontinentalitatszahl,
      feuchtezahl,
      reaktionszahl,
      nahrstoffzahl,
      strategie_c,
      strategie_r,
      strategie_s,
      lolium_multiflorum_p_a,
      veronica_filiformis_p_a,
      veronica_persica_p_a,
      medicago_sativa_p_a,
      erigeron_annuus_p_a,
      matricaria_discoidea_p_a,
      bromus_inermis_p_a,
      conyza_canadensis_aggr_p_a,
      impatiens_parviflora_p_a,
      juncus_tenuis_p_a,
      solidago_gigantea_p_a,
      vicia_villosa_p_a
    ) |>
    mutate(
      across(!ends_with("_p_a"), as.numeric),
      across(ends_with("_p_a"), as.logical),
    )
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
gpkg_path_resurvey <- "vectors_resurvey_test.gpkg"


resurvey <- read_csv("Squarefoot_data_long.csv")
#resurvey$dataset_id <- as.numeric(factor(resurvey$Time)) ########### 1 = delta, 3 = resurvey, 2 = historic 
#resurvey$Lebensraumgruppe <- resurvey$Time 

id_cols <- c("PAG")
independent_vars <- c("Time","Precision")#, "Lebensraumgruppe") #dataset_id
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
stopifnot(id_cols %in% colnames(resurvey) |> all())
stopifnot(independent_vars %in% colnames(resurvey) |> all())
stopifnot(dependent_vars %in% colnames(resurvey) |> all())
stopifnot(coordinate_cols %in% colnames(resurvey) |> all())




# Quality checks?







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
resurvey <- resurvey[,c(id_cols,independent_vars,dependent_vars, coordinate_cols)]  |>
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


aggregate_resurvey <- function(resurvey, vals, by){
  
  # browser()
  resurvey_vals <- resurvey |> 
    ungroup() |> 
    (\(x)x[,vals])()
  
  by_resurvey <- aggregate(resurvey_vals, by, FUN = mean,na.rm = TRUE)
  
  
  by_resurvey$n <- aggregate(resurvey[,1], by, FUN = length) |>
    st_drop_geometry() |> 
    (\(x) x[,1])()
  
  cbind(by_resurvey, st_drop_geometry(by))
}


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

hex10_resurvey <- aggregate_resurvey(resurvey, vals, hex10) |>
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




hex20_resurvey <- aggregate_resurvey(resurvey, vals, hex20) |> 
  st_transform(4326) |> 
  filter(!is.na(n))

bgr_resurvey <- aggregate_resurvey(resurvey, vals, BGR) |> 
  st_transform(4326) |> 
  filter(!is.na(n))

kantone_resurvey <- aggregate_resurvey(resurvey, vals, kantone) |> 
  st_transform(4326) |> 
  filter(!is.na(n))



if(file.exists(gpkg_path_resurvey))file.remove(gpkg_path_resurvey)
write_sf(hex10_resurvey, gpkg_path_resurvey, "hex10", delete_layer = TRUE)
write_sf(hex20_resurvey, gpkg_path_resurvey, "hex20", delete_layer = TRUE)
write_sf(bgr_resurvey, gpkg_path_resurvey, "bgr", delete_layer = TRUE)
write_sf(kantone_resurvey, gpkg_path_resurvey, "kantone", delete_layer = TRUE)

resurvey |> 
  st_transform(4326) |>
  write_sf(gpkg_path_resurvey, "punkte", delete_layer = TRUE)


layers <- tibble(layer_name = st_layers(gpkg_path_resurvey)$name)

cbind(
  st_drop_geometry(resurvey),
  st_coordinates(resurvey)
) |>
  write_csv("resurvey_test.csv")






#######################################################################################################################################
#check data

csv_path_res <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/grassland-data-main/appdata/resurvey.csv"
gpkg_path_res <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/grassland-data-main/appdata/vectors_resurvey.gpkg"
gpkg_path_tot <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/grassland-data-main/appdata/vectors.gpkg"

csv_path_sqft <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/resurvey_test.csv"
gpkg_path_sqft <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/vectors_resurvey_test.gpkg"


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
