

library("tidyverse")
library("sf")
library("readxl")
library("glue")
library("here")
setwd("C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/")

# theres multiple data to consider: dependent vars are darzustellende variablen
# in aggregate grass is the diversity that is plotted as different colors depending how big they are and how much they changed
# also in import sheet -> they import  "Grassland_ALLEMA-BDM-WBS_v.6_Biodiversitätsmonitor.xlsx", I need an equivalent
# sheets <- c(normallandschaft = "Normallandschaft (BDM)", tww = "TWW (WBS)", moore = "Moore (WBS)")
# drei levels, kann man diese drei optionen dann auf der webseite auswählen - wenn ja,
# könnte ich diese mit der Zeit hier ersetzen


# ToDO:
# - adjust aggregate_grass
# - adjust import_sheet
# - adjust dependent_vars
#adjust sheets to import



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

aggregate_grass <- function( # adjust columns_to_weight to my data? : ,"Therophyte_prop_HP","Geophyte_prop_HP","Hemicryptophyte_prop_HP",
  # "Herbaceous_chamaephyte_prop_HP","Richness_method_corr_RP","Simpson_RP"
  # TD: Taxonomic diversity
  # PD: Phylogenetic diversity
  # FD: Functional diversity
    x, 
    by, 
    weight_col = "design_weight", 
    columns_to_weight = c("artenreichtum_gefasspflanzen", 
                          "artenreichtum_neophyten", 
                          "artenanteil_neophyten",
                          "deckungsanteil_neophyten", 
                          "temperaturzahl", 
                          "kontinentalitatszahl",
                          "feuchtezahl", 
                          "reaktionszahl",
                          "nahrstoffzahl", 
                          "strategie_c", 
                          "strategie_r", 
                          "strategie_s"), 
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
      across(!ends_with("_p_a"),as.numeric),
      across(ends_with("_p_a"),as.logical),
    )
}


squarefoot <- read_csv("C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/Squarefoot_data_long.csv")

colnames(squarefoot)
id_cols <- c("PAG")
independen_vars <- c("Time")
dependen_vars <- c("Richness_method_corr",
                   "Simpson",
                   "PD",
                   "FD",
                   "FD_sla",
                   "FD_seed_mass",
                   "FD_height",
                   "T",
                   "N",
                   "R",
                   "F",
                   "L",
                   "MV",
                   "EM",
                   "Cover_Poaceae",
                   "Cover_Forb",
                   "Cover_Cyperaceae_Juncaceae",
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


cbind(
  st_drop_geometry(squarefoot_clean),
  st_coordinates(squarefoot_clean)
) |>
  write_csv("C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/sqft.csv")

gpkg_path <- "C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/vectors.gpkg"

## ↳ Prepare Spatial Aggregation Layers
################################################################################################################
# 
BGR <- read_sf("C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/biogreg/BiogeographischeRegionen/N2020_Revision_BiogeoRegion.shp") |>
   st_zm() 
 
kantone <- read_sf("C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp") |> 
  st_zm() |> 
   select(NAME, KANTONSNUM)
 
schweiz <- read_sf("C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp") |> 
   st_zm() |> 
   filter(NAME != "Liechtenstein") |> 
   st_union()
 
sheets <- c(normallandschaft = "Normallandschaft (BDM)", tww = "TWW (WBS)", moore = "Moore (WBS)")
 
xlsx_path <- "Grassland_ALLEMA-BDM-WBS_v.6_Biodiversitätsmonitor.xlsx"
#gpkg_path <- "vectors.gpkg"
# 
# # delete_all_layers(gpkg_path)
# # lays <- read_all_layers(gpkg_path)

grass_df <- imap(sheets, \(x,y){import_sheet(xlsx_path, x)})
 
 
 
grass_sf <- imap(grass_df, ~st_as_sf(.x, coords = c("x_lv95","y_lv95"), crs = 2056, remove = FALSE) )


hex10 <- st_make_grid(schweiz, 10000,square = FALSE) |> st_as_sf() |> mutate(hex10 = row_number())
hex20 <- st_make_grid(schweiz, 20000,square = FALSE) |> st_as_sf() |> mutate(hex20 = row_number())
 
BGR <- st_transform(BGR, st_crs(hex10)) ###########################################

hex10_BGR <- hexagonize(hex10, BGR, DERegionNa)

grass_sf <- map(grass_sf, ~ st_transform(.x, st_crs(hex10_BGR))) #################################


hex10_BGR_l <- imap(grass_sf, ~aggregate_grass(.x, hex10_BGR))
imap(hex10_BGR_l, function(x,y){x |> st_transform(4326) |> 
     write_sf(gpkg_path, glue("hex10_BGR_{y}"),delete_layer = TRUE)})
 
hex10_kantone <- hexagonize(hex10,kantone, NAME)
hex10_kantone_l <- imap(grass_sf, ~aggregate_grass(.x, hex10_kantone))
imap(hex10_kantone_l, function(x,y){x |> st_transform(4326) |> 
    write_sf(gpkg_path, glue("hex10_kantone_{y}"),delete_layer = TRUE)})
 
hex10_l <- imap(grass_sf, ~aggregate_grass(.x, hex10))
imap(hex10_l, function(x,y){x |> st_transform(4326) |> 
     write_sf(gpkg_path, glue("hex10_{y}"),delete_layer = TRUE)})
grass_sf <- map(grass_sf, \(x) st_join(x, hex10))
 
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
 
kantone_l <- imap(grass_sf, ~aggregate_grass(.x, kantone))
imap(kantone_l, function(x,y){x |> st_transform(4326) |> write_sf(gpkg_path, glue("kantone_{y}"),delete_layer = TRUE)})
grass_sf <- map(grass_sf, \(x) st_join(x, kantone))
 
 
aggregate_resurvey <- function(resuvey, by){
   by_resuvey <- aggregate(resuvey, by, FUN = mean,na.rm = TRUE)
   by_resuvey$n <- aggregate(resuvey[,1], by, FUN = length) |>
     st_drop_geometry() |> 
     (\(x) x[,1])()
   
   by_resuvey
 }
 

squarefoot_clean$time <- as.numeric(factor(squarefoot_clean$time)) ########### 1 = delta, 3 = resurvey, 2 = historic 
#hex10 <- st_transform(hex10, 2056) #################################################
hex10 <- st_transform(hex10, st_crs(squarefoot_clean)) #################################################
hex20 <- st_transform(hex20, st_crs(squarefoot_clean)) #################################################
BGR <- st_transform(BGR, st_crs(squarefoot_clean)) #################################################
kantone <- st_transform(kantone, st_crs(squarefoot_clean)) #################################################

hex10_resurvey <- aggregate_resurvey(squarefoot_clean, hex10) |> st_transform(4326)
hex20_resurvey <- aggregate_resurvey(squarefoot_clean, hex20) |> st_transform(4326)
bgr_resurvey <- aggregate_resurvey(squarefoot_clean, BGR) |> st_transform(4326)
kantone_resurvey <- aggregate_resurvey(squarefoot_clean, kantone) |> st_transform(4326)

write_sf(hex10_resurvey, gpkg_path, "hex10_resurvey", delete_layer = TRUE)
write_sf(hex20_resurvey, gpkg_path, "hex20_resurvey", delete_layer = TRUE)
write_sf(bgr_resurvey, gpkg_path, "bgr_resurvey", delete_layer = TRUE)
write_sf(kantone_resurvey, gpkg_path, "kantone_resurvey", delete_layer = TRUE)



layers <- tibble(layer_name = st_layers(gpkg_path)$name)

layers <- layers |>
  extract(layer_name,c("aggregation","dataset"),"(\\w+)_(\\w+)",remove = FALSE) |>
  separate(aggregation, c("aggregation1","aggregation2"),sep = "_",fill = "right")

write_sf(layers, gpkg_path, "layers_overview",  delete_layer = TRUE)


grass_sf |>
  imap(\(x,y){
    x |>
      select(-ends_with("lv95")) |>
      mutate(across(c(lange,breite),\(z)round(z,2))) |>
      st_drop_geometry() |>
      write_csv(glue("{y}.csv"))
  })












## ↳ Prepare Spatial Aggregation Layers
################################################################################################################
# 
# hex5 <- st_make_grid(schweiz, 5000, square = FALSE) |>
#   st_as_sf() |>
#   mutate(hex5 = row_number())
# 
# hex10 <- st_make_grid(schweiz, 10000, square = FALSE) |>
#   st_as_sf() |>
#   mutate(hex10 = row_number())
# 
# hex20 <- st_make_grid(schweiz, 20000, square = FALSE) |>
#   st_as_sf() |>
#   mutate(hex20 = row_number())
# 
# #st_crs(hex10)    # check CRS of hex grid
# #st_crs(BGR)   
# BGR <- st_transform(BGR, st_crs(hex10)) ###########################################
# 
# hex10_BGR <- hexagonize(hex10, BGR, DERegionNa)
# 
# BGR <- BGR |>
#   group_by(bgr = DERegionNa) |>
#   summarise()
# 
# 
# kantone <- kantone |>
#   group_by(kantone = NAME) |>
#   summarise()
# 
# 
# ## ↳ Aggregate BDM to Polygon and export to gpkg
# grass_sf <- map(grass_sf, ~ st_transform(.x, st_crs(hex10_BGR))) #################################
# 
# hex10_BGR_l <- imap(grass_sf, ~ aggregate_grass(.x, hex10_BGR))
# 
# imap(hex10_BGR_l, function(x, y) {
#   x |>
#     st_transform(4326) |>
#     write_sf(gpkg_path, glue("hex10_BGR_{y}"), delete_layer = TRUE)
# })
# 
# hex10_kantone <- hexagonize(hex10, kantone)#, NAME) ###################3##########
# 
# hex10_kantone_l <- imap(grass_sf, ~ aggregate_grass(.x, hex10_kantone))
# 
# imap(hex10_kantone_l, function(x, y) {
#   x |>
#     st_transform(4326) |>
#     write_sf(gpkg_path, glue("hex10_kantone_{y}"), delete_layer = TRUE)
# })
# 
# hex10_l <- imap(grass_sf, ~ aggregate_grass(.x, hex10))
# 
# imap(hex10_l, function(x, y) {
#   x |>
#     st_transform(4326) |>
#     write_sf(gpkg_path, glue("hex10_{y}"), delete_layer = TRUE)
# })
# grass_sf <- map(grass_sf, \(x) st_join(x, hex10))
# 
# hex20_l <- imap(grass_sf, ~ aggregate_grass(.x, hex20))
# 
# imap(hex20_l, function(x, y) {
#   x |>
#     st_transform(4326) |>
#     write_sf(gpkg_path, glue("hex20_{y}"), delete_layer = TRUE)
# })
# grass_sf <- map(grass_sf, \(x) st_join(x, hex20))
# 
# 
# BGR_l <- imap(grass_sf, ~ aggregate_grass(.x, BGR))
# 
# imap(BGR_l, function(x, y) {
#   x |>
#     st_transform(4326) |>
#     write_sf(gpkg_path, glue("BGR_{y}"), delete_layer = TRUE)
# })
# 
# grass_sf <- map(grass_sf, \(x) st_join(x, BGR))
# 
# 
# 
# kantone_l <- imap(grass_sf, ~ aggregate_grass(.x, kantone))
# 
# imap(kantone_l, function(x, y) {
#   x |>
#     st_transform(4326) |>
#     write_sf(gpkg_path, glue("kantone_{y}"), delete_layer = TRUE)
# })
# 
# grass_sf <- map(grass_sf, \(x) st_join(x, kantone))
# 
# layers <- tibble(layer_name = st_layers(gpkg_path)$name)
# 
# layers <- layers |>
#   extract(layer_name, c("aggregation", "dataset"), "(\\w+)_(\\w+)", remove = FALSE) |>
#   separate(aggregation, c("aggregation1", "aggregation2"), sep = "_", fill = "right")
# 
# write_sf(layers, gpkg_path, "layers_overview")
# 
# 
# ## ↳ Resurvey: Aggregate Data to Polygon and export to gpkg
# 
# threshold <- read_xlsx("tmp_resurvey_2025-01_annual_trends_threshold_definitions.xlsx", "Schwellenwerte")

# 
# hex10_resurvey <- aggregate_resurvey(resurvey_smry, vals, hex10) |> 
#   st_transform(4326) |> 
#   filter(!is.na(n))
# 
# 
# # hex10_resurvey[,!(colnames(hex10_resurvey) %in% threshold$Parameter)]
# # 
# # threshold_long <- threshold |>
# #   pivot_longer(-Parameter)
# # 
# # map(threshold$Parameter, \(x){
# #   browser()
# # 
# #   thr <- pivot_longer(threshold[threshold$Parameter == x, ], -Parameter)
# #   cut(hex10_resurvey[,x, drop = TRUE], c(-Inf, thr$value))
# # })
# 
# 

# 
# hex20_resurvey <- aggregate_resurvey(resurvey_smry, vals, hex20) |> 
#   st_transform(4326) |> 
#   filter(!is.na(n))
# 
# bgr_resurvey <- aggregate_resurvey(resurvey_smry, vals, BGR) |> 
#   st_transform(4326) |> 
#   filter(!is.na(n))
# 
# kantone_resurvey <- aggregate_resurvey(resurvey_smry, vals, kantone) |> 
#   st_transform(4326) |> 
#   filter(!is.na(n))

# 
# 
# if(file.exists(gpkg_path_resurvey))file.remove(gpkg_path_resurvey)
# write_sf(hex10_resurvey, gpkg_path_resurvey, "hex10", delete_layer = TRUE)
# write_sf(hex20_resurvey, gpkg_path_resurvey, "hex20", delete_layer = TRUE)
# write_sf(bgr_resurvey, gpkg_path_resurvey, "bgr", delete_layer = TRUE)
# write_sf(kantone_resurvey, gpkg_path_resurvey, "kantone", delete_layer = TRUE)
# 
# resurvey_smry |> 
#   st_transform(4326) |>
#   write_sf(gpkg_path_resurvey, "punkte", delete_layer = TRUE)
# 
# 
# layers <- tibble(layer_name = st_layers(gpkg_path_resurvey)$name)
# 
# cbind(
#   st_drop_geometry(resurvey_smry),
#   st_coordinates(resurvey_smry)
# ) |>
#   write_csv("appdata/resurvey.csv")
# 

