

library("tidyverse")
library("sf")
library("readxl")
library("glue")
library("here")
setwd("C:/Users/hedd/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/")


# there is multiple data to consider: dependent vars are darzustellende variablen
# in aggregate grass is the diversity that is plotted as different colors depending how big they are and how much they changed
# also in import sheet -> they import  "Grassland_ALLEMA-BDM-WBS_v.6_Biodiversitätsmonitor.xlsx", I need an equivalent
# sheets <- c(normallandschaft = "Normallandschaft (BDM)", tww = "TWW (WBS)", moore = "Moore (WBS)")
# drei levels, kann man diese drei optionen dann auf der webseite auswählen - wenn ja,
# könnte ich diese mit der Zeit hier ersetzen

# was ist der Unterschied von "grassland-data-raw/Grassland_ALLEMA-BDM-WBS_v.6_Biodiversitätsmonitor.xlsx" und 
# "grassland-data-raw/Dashboard_Resurveys_v.01.xlsx" - wieso zwei files? - bruuch ich BDM? fürs grass_df
# warum zwei verschiedene files? eine isch ufteilt in moore, normallandschaft und tww, de anderi nöd

# was macht aggregate_grass?

# in was für einer form müssen meine daten sein? ich habe sie nun alle in einem file, jedes sheet ist eine 
# Zeit der drei Auswahlmöglichkeiten.

# was sind diese variablen bei import_sheet?

# was ist das ganze mit "lv95", zb in grass_sf am schluss?

# wenni skript mit resurvey laufe lah chunnt error bi hexagonize:
# hex10_BGR <- hexagonize(hex10, BGR, DERegionNa)
# Fehler in geos_op2_geom("intersection", x, y, ...) : 
#  st_crs(x) == st_crs(y) ist nicht TRUE

# bei aggregate(resurvey_clean, hex10, FUN = mean,na.rm = TRUE)
# gibt es nur NA weil warnung: In mean.default(X[[i]], ...) :
# Argument ist weder numerisch noch boolesch: gebe NA zurück
# es scheind geometry nicht zu erkennen

# brauchen wir die aggregationen kantone x hex10, BGR x hex10 überhaupt?

# wo kommt precision ins spiel? - erst beim app teil und noch nicht bei der preparation?

#was macht design_weight und brauche ich das?

# was macht aggregate_squarefoot und was ist grass_sf am schluss?


# TODO:
# - adjust code from data_preparation zeitreihen?
# - adjust aggregate_grass
# - adjust import_sheet
# - adjust dependent_vars
# - adjust sheets to import
# - stefan fröge wellli variable wo hi ghöred, bzw welles die dargestellte variable sind



read_all_layers <- function(file){
  sapply(st_layers(file)$name, \(x)st_read(file, x),simplify = FALSE) 
}

delete_all_layers <- function(file){
  sapply(st_layers(file)$name, \(x)st_delete(file, x),simplify = FALSE) 
}

hexagonize <- function(hex, to_be_hexagonized, ..., .na_omit = TRUE, .do_union = TRUE){
  joined <- st_join(hex, select(to_be_hexagonized, ...), largest = TRUE)
  if(.na_omit) joined <- na.omit(joined)
  if(.do_union){
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
    columns_to_weight = c("species_richness",
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
                          "csr_competitive_ability"), 
    columns_to_count = "_p_a$", # columns matching this ("matches()") will just be counted
    filter_zero_count = TRUE){
  
  # browser()

  stopifnot(all(columns_to_weight %in%names(x)))

  stopifnot(weight_col %in% names(x))

  # create a tidy select that I can use with matches()
  columns_to_weight_tidy_sel <- paste(columns_to_weight, collapse = "|")

  # multiply all columns that should be weighed with the weight_col
  weighted <- x[c(weight_col, columns_to_weight)] |>  # select only the weight col and the columns to weigh
    mutate(
      across(
        matches(columns_to_weight_tidy_sel),   # for all columns that should be weighed...
        function(z){z*x[[weight_col]]})        # ... multiply by the weigh col
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
        function(z){z/weighted_sum[[weight_col]]})
    ) 
  
  weighted_mean$n <- aggregate(transmute(x, n = 1), by, FUN = length) |> st_drop_geometry() |> (\(x) x[,1])()
  weighted_mean <- weighted_mean|> 
    mutate(
      across(
        ends_with("_p_a"),
        function(z){z/n}
      )
    )

  weighted_mean <- cbind(
    weighted_mean,
    st_drop_geometry(by)
  )

  if(filter_zero_count){
    weighted_mean <- weighted_mean[weighted_mean$n>0 & !is.na(weighted_mean$n),]
  }
  weighted_mean
}

import_sheet <- function(xlsx, sheet){
  library(readxl)
  library(janitor)
  library(dplyr)
  read_excel(xlsx, sheet) |>
    janitor::clean_names() |>
    transmute(
      pag,
      time,
      precision,
      species_richness,
      phylogenetic_diversity,
      functional_diversity,
      funct_div_spec_leaf_area,
      funct_div_seed_mass,
      funct_div_height,
      temperature,
      nutrient,
      reaction,
      moisture,
      light,
      moving_tolerance,
      urbanization,
      cover_poaceae,
      cover_forb,
      cover_cyp_junc,
      csr_stress_tolerance,
      csr_disturbance_tolerance,
      csr_competitive_ability,
      x,
      y,
      design_weight
      ) |> 
     mutate(
       across(!ends_with("_p_a"),as.numeric),
       across(ends_with("_p_a"),as.logical),
     )
 }


squarefoot <- read_csv("Squarefoot_data_long.csv")

colnames(squarefoot)
id_cols <- c("PAG")
independen_vars <- c("Time","Precision")
dependen_vars <- c("Species_richness",
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
coordinate_cols <- c("Center_x_coordinate", "Center_y_coordinate")


id_cols %in% colnames(squarefoot) |> all()
independen_vars %in% colnames(squarefoot) |> all()
id_cols %in% colnames(squarefoot) |> all()
dependen_vars %in% colnames(squarefoot) |> all()
coordinate_cols %in% colnames(squarefoot) |> all()


squarefoot_clean <- squarefoot[,c(id_cols,independen_vars,dependen_vars, coordinate_cols)]  |>
  filter(if_any(matches(coordinate_cols), \(x)!is.na(x))) |>
  st_as_sf(coords = coordinate_cols, crs = 2056) |>
  janitor::clean_names()

# add design_weight
squarefoot_clean$design_weight <- 1

# categorize time variable to have it numeric
squarefoot_clean$time <- as.numeric(factor(squarefoot_clean$time)) ########### 1 = delta, 3 = resurvey, 2 = historic 


library(writexl)
data <- cbind(
  st_drop_geometry(squarefoot_clean),
  st_coordinates(squarefoot_clean))
write_csv(data, "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/sqft_clean.csv") #statt yaelh hedd
write_xlsx(data, "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/sqft_clean.xlsx")
# squarefoot_clean <- cbind(
#   st_drop_geometry(squarefoot_clean),
#   st_coordinates(squarefoot_clean))



## ↳ Prepare Spatial Aggregation Layers
################################################################################################################
# 
BGR <- read_sf("C:/Users/hedd/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/biogreg/BiogeographischeRegionen/N2020_Revision_BiogeoRegion.shp") |>
   st_zm() 
 
kantone <- read_sf("C:/Users/hedd/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp") |> 
  st_zm() |> 
   select(NAME, KANTONSNUM)
 
schweiz <- read_sf("C:/Users/hedd/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp") |> 
   st_zm() |> 
   filter(NAME != "Liechtenstein") |> 
   st_union()
 
#sheets <- c(normallandschaft = "Normallandschaft (BDM)", tww = "TWW (WBS)", moore = "Moore (WBS)")
sheets <- c(sqft_data = "Sheet1")
  
#xlsx_path <- "Grassland_ALLEMA-BDM-WBS_v.6_Biodiversitätsmonitor.xlsx"
xlsx_path <- "sqft_clean.xlsx"
#gpkg_path <- "vectors.gpkg"
gpkg_path <- "C:/Users/hedd/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/vectors_test.gpkg"

# # delete_all_layers(gpkg_path)
# # lays <- read_all_layers(gpkg_path)

grass_df <- imap(sheets, \(x,y){import_sheet(xlsx_path, x)})
#grass_df <- squarefoot_clean
 
 
grass_sf <- imap(grass_df, ~st_as_sf(.x, coords = c("x","y"), crs = 2056, remove = FALSE) )


hex10 <- st_make_grid(schweiz, 10000,square = FALSE) |> st_as_sf() |> mutate(hex10 = row_number())
hex20 <- st_make_grid(schweiz, 20000,square = FALSE) |> st_as_sf() |> mutate(hex20 = row_number())
 
hex10 <- st_transform(hex10, crs = st_crs(BGR))#####################################################

hex10_BGR <- hexagonize(hex10, BGR, DERegionNa)
hex10_BGR_l <- imap(grass_sf, ~aggregate_grass(.x, hex10_BGR))
imap(hex10_BGR_l, function(x,y){x |> st_transform(4326) |> 
     write_sf(gpkg_path, glue("hex10_BGR_{y}"),delete_layer = TRUE)})

hex10 <- st_transform(hex10, crs = st_crs(kantone))#####################################################

hex10_kantone <- hexagonize(hex10,kantone, NAME)

hex10_kantone <- st_transform(hex10_kantone, crs = st_crs(BGR))#####################################################

hex10_kantone_l <- imap(grass_sf, ~aggregate_grass(.x, hex10_kantone))
imap(hex10_kantone_l, function(x,y){x |> st_transform(4326) |> 
    write_sf(gpkg_path, glue("hex10_kantone_{y}"),delete_layer = TRUE)})

hex10 <- st_transform(hex10, crs = st_crs(BGR))#####################################################
 
hex10_l <- imap(grass_sf, ~aggregate_grass(.x, hex10))
imap(hex10_l, function(x,y){x |> st_transform(4326) |> 
     write_sf(gpkg_path, glue("hex10_{y}"),delete_layer = TRUE)})
grass_sf <- map(grass_sf, \(x) st_join(x, hex10))

hex20 <- st_transform(hex20, crs = st_crs(BGR))#####################################################

hex20_l <- imap(grass_sf, ~aggregate_grass(.x, hex20))
imap(hex20_l, function(x,y){x |> st_transform(4326) |> 
     write_sf(gpkg_path, glue("hex20_{y}"),delete_layer = TRUE)})
grass_sf <- map(grass_sf, \(x) st_join(x, hex20))
 
 
BGR <- BGR |> 
   group_by(BGR = DERegionNa) |> 
   summarise()
 
 
BGR_l <- imap(grass_sf, ~aggregate_grass(.x, BGR))
imap(BGR_l, function(x,y){x |> st_transform(4326) |> write_sf(gpkg_path, glue("BGR_{y}"),delete_layer = TRUE)})
grass_sf <- map(grass_sf, \(x) st_join(x, BGR))
 
 
kantone <- kantone |> 
   group_by(kantone = NAME) |> 
   summarise()
kantone <- st_transform(kantone, crs = st_crs(BGR))#####################################################

kantone_l <- imap(grass_sf, ~aggregate_grass(.x, kantone))
imap(kantone_l, function(x,y){x |> st_transform(4326) |> write_sf(gpkg_path, glue("kantone_{y}"),delete_layer = TRUE)})
grass_sf <- map(grass_sf, \(x) st_join(x, kantone))
 


# TODO:
# only NA get out of the aggregate_squarefoot function
# aggregate_squarefoot <- function(sqfoot, by){
#    by_sqfoot <- aggregate(sqfoot, by, FUN = mean,na.rm = TRUE)
#    by_sqfoot$n <- aggregate(sqfoot[,1], by, FUN = length) |>
#      st_drop_geometry() |> 
#      (\(x) x[,1])()
#    
#    by_squarefoot
#  }
aggregate_squarefoot <- function(sqfoot, by){ ###################################################################
  # Punkte den Polygonen zuordnen
  by_sqfoot <- st_join(sqfoot, by, left = FALSE)
  print(by)
  # Numerische Spalten aggregieren + n Punkte pro Polygon
  by_sqfoot <- by_sqfoot %>%
    group_by(names(by)[2]) %>%                       
    summarise(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      n = n()
    )
  by_sqfoot
 }######################################################################################################################################

 
#troubleshooting
############################################

# by_sqfoot_test <- aggregate(squarefoot_clean, hex10, FUN = mean, na.rm = TRUE)
# sapply(squarefoot_clean, class) #gleicher crs?
# table(st_within(squarefoot_clean, hex10, sparse = FALSE)) #liegen die datenpunkte innerhalb der polygone?
# st_bbox(squarefoot_clean)
# st_bbox(hex10)
# # Logical Matrix: TRUE, wenn Punkt i in Polygon j liegt
# within_matrix <- st_within(squarefoot_clean, hex10, sparse = FALSE)
# 
# # Anzahl Punkte ohne Zuordnung
# sum(rowSums(within_matrix) == 0)
# 
# hex10_t <- st_transform(hex10, 3857)             #.
# squarefoot_clean_t <- st_transform(squarefoot_clean, 3857)
# by_sqfoot_test <- aggregate(squarefoot_clean_t, hex10_t, FUN = mean, na.rm = TRUE)
# 
# st_within(squarefoot_clean[1:10,], hex10[1,], sparse = FALSE)
# library(ggplot2)
# 
# ggplot() +
#   geom_sf(data = hex10, fill = NA, color = "red") +
#   geom_sf(data = squarefoot_clean, color = "blue", size = 0.5)
# hex10_buffer <- st_buffer(hex10, 1)  # 1 Einheit Puffern
# st_within(squarefoot_clean, hex10_buffer, sparse = FALSE)
# 
# sum(!st_is_valid(hex10))
# sum(!st_is_valid(squarefoot_clean))
# 
# hex10_t <- st_make_valid(hex10)
# squarefoot_clean_t <- st_make_valid(squarefoot_clean)
# st_within(squarefoot_clean_t[1:10,], hex10_t[1,], sparse = FALSE)
# sum(!st_is_valid(hex10_t))
# sum(!st_is_valid(squarefoot_clean_t))
# 
# joined <- st_join(squarefoot_clean, hex10, left = FALSE)
# by_sqfoot_test <- joined %>%
#   group_by(hex10) %>%
#   summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
# 
# length(squarefoot_clean$pag)
# length(by_sqfoot_test$pag)
# 
# # Punkte den Polygonen zuordnen
# joined <- st_join(squarefoot_clean, hex10, left = FALSE)
# 
# # Numerische Spalten aggregieren + n Punkte pro Polygon
# by_sqfoot <- joined %>%
#   group_by(hex10) %>%                       # Ersetze hex_id ggf. durch die ID-Spalte in 'by'
#   summarise(
#     across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
#     n = n()
#   )

################################################



hex10_squarefoot <- aggregate_squarefoot(squarefoot_clean, hex10) |> st_transform(4326)
hex20_squarefoot <- aggregate_squarefoot(squarefoot_clean, hex20) |> st_transform(4326)
bgr_squarefoot <- aggregate_squarefoot(squarefoot_clean, BGR) |> st_transform(4326)
kantone_squarefoot <- aggregate_squarefoot(squarefoot_clean, kantone) |> st_transform(4326)

write_sf(hex10_squarefoot, gpkg_path, "hex10_squarefoot", delete_layer = TRUE)
write_sf(hex20_squarefoot, gpkg_path, "hex20_squarefoot", delete_layer = TRUE)
write_sf(bgr_squarefoot, gpkg_path, "bgr_squarefoot", delete_layer = TRUE)
write_sf(kantone_squarefoot, gpkg_path, "kantone_squarefoot", delete_layer = TRUE)

layers <- tibble(layer_name = st_layers(gpkg_path)$name)

layers <- layers |>
  extract(layer_name,c("aggregation","dataset"),"(\\w+)_(\\w+)",remove = FALSE) |>
  separate(aggregation, c("aggregation1","aggregation2"),sep = "_",fill = "right")

write_sf(layers, gpkg_path, "layers_overview",  delete_layer = TRUE)









#######################################################################################################################################
#check data

csv_path_res <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/grassland-data-main/appdata/resurvey.csv"
gpkg_path_res <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/grassland-data-main/appdata/vectors_resurvey.gpkg"
gpkg_path_tot <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/grassland-data-main/appdata/vectors.gpkg"

csv_path_sqft <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/sqft_clean.csv"
gpkg_path_sqft <- "C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/vectors_test.gpkg"


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








# imap(grass_df, ~write_csv(.x, glue("appdata/{.y}.csv")))
# this following line reduces the precision to 2 decimal places 
# in WGS84 is about 1'500m (+/- 750m?). With this approach, the 
# raw data can basically be made publicly accessible (I need to confirm this
# with deng). It's important to do this rounding on the basis on WGS84 coordinates
# since the coordinates in 2056 are in some cases already a round number (since
# they originate from a "grid" placed over Switzerland in 21781 or 2056)
# grass_sf |> 
#   imap(\(x,y){
#     x |> 
#       select(-ends_with("lv95")) |> 
#       mutate(across(c(lange,breite),\(z)round(z,2))) |> 
#       st_drop_geometry() |> 
#       write_csv(glue("appdata/{y}.csv"))
#   })




# ## ↳ squarefoot: Aggregate Data to Polygon and export to gpkg
# 
# threshold <- read_xlsx("tmp_resurvey_2025-01_annual_trends_threshold_definitions.xlsx", "Schwellenwerte")

# 
# hex10_squarefoot <- aggregate_squarefoot(squarefoot_smry, vals, hex10) |> 
#   st_transform(4326) |> 
#   filter(!is.na(n))
# 
# 
# # hex10_squarefoot[,!(colnames(hex10_squarefoot) %in% threshold$Parameter)]
# # 
# # threshold_long <- threshold |>
# #   pivot_longer(-Parameter)
# # 
# # map(threshold$Parameter, \(x){
# #   browser()
# # 
# #   thr <- pivot_longer(threshold[threshold$Parameter == x, ], -Parameter)
# #   cut(hex10_squarefoot[,x, drop = TRUE], c(-Inf, thr$value))
# # })
# 
# 

# 
# hex20_squarefoot <- aggregate_squarefoot(squarefoot_smry, vals, hex20) |> 
#   st_transform(4326) |> 
#   filter(!is.na(n))
# 
# bgr_squarefoot <- aggregate_squarefoot(squarefoot_smry, vals, BGR) |> 
#   st_transform(4326) |> 
#   filter(!is.na(n))
# 
# kantone_squarefoot <- aggregate_squarefoot(squarefoot_smry, vals, kantone) |> 
#   st_transform(4326) |> 
#   filter(!is.na(n))

# 
# 
# if(file.exists(gpkg_path_squarefoot))file.remove(gpkg_path_squarefoot)
# write_sf(hex10_squarefoot, gpkg_path_squarefoot, "hex10", delete_layer = TRUE)
# write_sf(hex20_squarefoot, gpkg_path_squarefoot, "hex20", delete_layer = TRUE)
# write_sf(bgr_squarefoot, gpkg_path_squarefoot, "bgr", delete_layer = TRUE)
# write_sf(kantone_squarefoot, gpkg_path_squarefoot, "kantone", delete_layer = TRUE)
# 
# squarefoot_smry |> 
#   st_transform(4326) |>
#   write_sf(gpkg_path_squarefoot, "punkte", delete_layer = TRUE)
# 
# 
# layers <- tibble(layer_name = st_layers(gpkg_path_squarefoot)$name)
# 
# cbind(
#   st_drop_geometry(squarefoot_smry),
#   st_coordinates(squarefoot_smry)
# ) |>
#   write_csv("appdata/squarefoot.csv")
# 

