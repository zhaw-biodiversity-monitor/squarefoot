

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



################################################################################################################
## Import Data
################################################################################################################


## ↳ Generic Geodata
################################################################################################################

BGR <- read_sf("data-raw/biogreg/BiogeographischeRegionen/N2020_Revision_BiogeoRegion.shp") |>
  st_zm()


kantone <- read_sf("data-raw/swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp") |>
  st_zm() |>
  select(NAME, KANTONSNUM) |>
  st_set_crs(2056)

schweiz <- read_sf("data-raw/swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp") |>
  st_zm() |>
  filter(NAME != "Liechtenstein") |>
  st_union() |>
  st_set_crs(2056)



## ↳ BDM (BioDiversityMonitor) Data
################################################################################################################

sheets <- c(
  normallandschaft = "Normallandschaft (BDM)"
  # tww = "TWW (WBS)",
  # moore = "Moore (WBS)"
)

xlsx_path <- "data-raw/Grassland_ALLEMA-BDM-WBS_v.6_Biodiversitätsmonitor.xlsx"
gpkg_path <- "appdata/vectors.gpkg"


# delete_all_layers(gpkg_path)
# lays <- read_all_layers(gpkg_path)

grass_df <- imap(sheets, \(x, y){
  import_sheet(xlsx_path, x)
})



grass_sf <- imap(grass_df, ~ st_as_sf(.x, coords = c("x_lv95", "y_lv95"), crs = 2056, remove = FALSE))



## ↳ Resurvey Data
################################################################################################################





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
cbind(
  st_drop_geometry(squarefoot_clean),
  st_coordinates(squarefoot_clean)
) |>
  #write_csv("C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/squarefoot/sqft_clean.csv")
  write_xlsx("C:/Users/hedd/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/sqft_clean.xlsx")
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


## ↳ Aggregate BDM to Polygon and export to gpkg
################################################################################################################

#dir.create("appdata")



hex10_BGR_l <- imap(grass_sf, ~ aggregate_grass(.x, hex10_BGR))

imap(hex10_BGR_l, function(x, y) {
  x |>
    st_transform(4326) |>
    write_sf(gpkg_path, glue("hex10_BGR_{y}"), delete_layer = TRUE)
})

hex10_kantone <- hexagonize(hex10, kantone, NAME)

hex10_kantone_l <- imap(grass_sf, ~ aggregate_grass(.x, hex10_kantone))

imap(hex10_kantone_l, function(x, y) {
  x |>
    st_transform(4326) |>
    write_sf(gpkg_path, glue("hex10_kantone_{y}"), delete_layer = TRUE)
})

hex10_l <- imap(grass_sf, ~ aggregate_grass(.x, hex10))

imap(hex10_l, function(x, y) {
  x |>
    st_transform(4326) |>
    write_sf(gpkg_path, glue("hex10_{y}"), delete_layer = TRUE)
})
grass_sf <- map(grass_sf, \(x) st_join(x, hex10))

hex20_l <- imap(grass_sf, ~ aggregate_grass(.x, hex20))

imap(hex20_l, function(x, y) {
  x |>
    st_transform(4326) |>
    write_sf(gpkg_path, glue("hex20_{y}"), delete_layer = TRUE)
})
grass_sf <- map(grass_sf, \(x) st_join(x, hex20))


BGR_l <- imap(grass_sf, ~ aggregate_grass(.x, BGR))

imap(BGR_l, function(x, y) {
  x |>
    st_transform(4326) |>
    write_sf(gpkg_path, glue("BGR_{y}"), delete_layer = TRUE)
})

grass_sf <- map(grass_sf, \(x) st_join(x, BGR))



kantone_l <- imap(grass_sf, ~ aggregate_grass(.x, kantone))

imap(kantone_l, function(x, y) {
  x |>
    st_transform(4326) |>
    write_sf(gpkg_path, glue("kantone_{y}"), delete_layer = TRUE)
})

grass_sf <- map(grass_sf, \(x) st_join(x, kantone))

layers <- tibble(layer_name = st_layers(gpkg_path)$name)

layers <- layers |>
  extract(layer_name, c("aggregation", "dataset"), "(\\w+)_(\\w+)", remove = FALSE) |>
  separate(aggregation, c("aggregation1", "aggregation2"), sep = "_", fill = "right")

write_sf(layers, gpkg_path, "layers_overview")


## ↳ Resurvey: Aggregate Data to Polygon and export to gpkg
################################################################################################################

threshold <- read_xlsx("data-raw/tmp_resurvey_2025-01_annual_trends_threshold_definitions.xlsx", "Schwellenwerte")


hex10_resurvey <- aggregate_resurvey(resurvey_smry, vals, hex10) |> 
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




hex20_resurvey <- aggregate_resurvey(resurvey_smry, vals, hex20) |> 
  st_transform(4326) |> 
  filter(!is.na(n))

bgr_resurvey <- aggregate_resurvey(resurvey_smry, vals, BGR) |> 
  st_transform(4326) |> 
  filter(!is.na(n))

kantone_resurvey <- aggregate_resurvey(resurvey_smry, vals, kantone) |> 
  st_transform(4326) |> 
  filter(!is.na(n))



if(file.exists(gpkg_path_resurvey))file.remove(gpkg_path_resurvey)
write_sf(hex10_resurvey, gpkg_path_resurvey, "hex10", delete_layer = TRUE)
write_sf(hex20_resurvey, gpkg_path_resurvey, "hex20", delete_layer = TRUE)
write_sf(bgr_resurvey, gpkg_path_resurvey, "bgr", delete_layer = TRUE)
write_sf(kantone_resurvey, gpkg_path_resurvey, "kantone", delete_layer = TRUE)

resurvey_smry |> 
  st_transform(4326) |>
  write_sf(gpkg_path_resurvey, "punkte", delete_layer = TRUE)


layers <- tibble(layer_name = st_layers(gpkg_path_resurvey)$name)

cbind(
  st_drop_geometry(resurvey_smry),
  st_coordinates(resurvey_smry)
) |>
  write_csv("appdata/resurvey.csv")
