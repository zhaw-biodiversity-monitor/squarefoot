# clean the raw data


# Abbreviations:
# HP: Historical plot
# RP: Resurvey plot
# PA: Potential area
# d: detla = Mean value PA RP - Mean value PA HP
# 
# TD: Taxonomic diversity
# PD: Phylogenetic diversity
# FD: Functional diversity
# sla: specific leaf area
# height: plant height
# CWM: community-weighted mean
# EIV: ecological indicator value
# T: CWM EIV temperature
# L: CWM EIV light
# F: CWM EIV moisture
# R: CWM EIV reaction
# N: CWM EIV nutrients
# MV: CWM EIV mowing tolerance
# EM: CWM EIV hemeroby



## combine data from two data files
data_1 <- read.csv("C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/SqFt_HP_env.csv", sep =";")
data_2 <- read.csv("C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/PAG_dDiv.csv", sep=";")


# length(data_1$PAG)
# length(data_2$PAG)
# length(unique(data_1$PAG))
# length(unique(data_2$PAG))

data_1 <- data_1[c("PAG", "Precision", "Center_x_coordinate", "Center_y_coordinate", "Canton", "Municipality")]
data_1_unique <- data_1[!duplicated(data_1$PAG), ]
merged_data <- merge(data_1_unique, data_2, by = c("PAG"))

# delete unused data columns
# sla_HP, height_log10_HP, seed_mass_log10_HP
# Therophyte_prop_HP, Geophyte_prop_HP, Hemicryptophyte_prop_HP, Herbaceous_chamaephyte_prop_HP
# sla_RP, height_log10_RP, seed_mass_log10_RP
# Therophyte_prop_RP, Geophyte_prop_RP, Hemicryptophyte_prop_RP, Herbaceous_chamaephyte_prop_RP
# Distance_RP_RP, Distance_HP_RP
# dTherophyte, dGeophyte, dHemicryptophyte, dHerbaceous_chamaephyte, dsla, dplant_heigt, dseed_mass

merged_data_subset <- merged_data[ , !(names(merged_data) %in% c("sla_HP", "height_log10_HP", "seed_mass_log10_HP",
                                                                  "Therophyte_prop_HP", "Geophyte_prop_HP", "Hemicryptophyte_prop_HP", "Herbaceous_chamaephyte_prop_HP",
                                                                  "sla_RP", "height_log10_RP", "seed_mass_log10_RP",
                                                                  "Therophyte_prop_RP", "Geophyte_prop_RP", "Hemicryptophyte_prop_RP", "Herbaceous_chamaephyte_prop_RP",
                                                                  "Distance_RP_RP", "Distance_HP_RP",
                                                                  "dTherophyte", "dGeophyte", "dHemicryptophyte", "dHerbaceous_chamaephyte", "dsla", "dplant_heigt", "dseed_mass"))]


write.csv(merged_data, "C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/squarefoot/Squarefoot_data.csv", row.names = FALSE)


###########################################################################################
# transform data time format wide to long - where the time variable is categorical
colnames(merged_data_subset)

# historic data
HP_subset <- merged_data_subset[ , grepl("_HP$", names(merged_data_subset))]
names(HP_subset) <- substr(names(HP_subset), 1, nchar(names(HP_subset)) - 3) # cut off last 3 letters to later overlap datasets
HP_subset <- cbind(merged_data_subset[ , c("PAG", "Precision", "Center_x_coordinate", "Center_y_coordinate", "Elevation", "Canton", "Municipality")],
                   HP_subset)
HP_subset["Time"] <- "historic"



# resurvey data
RP_subset <- merged_data_subset[ , grepl("_RP$", names(merged_data_subset))]
names(RP_subset) <- substr(names(RP_subset), 1, nchar(names(RP_subset)) - 3) # cut off last 3 letters to later overlap datasets
RP_subset <- cbind(merged_data_subset[ , c("PAG", "Precision", "Center_x_coordinate", "Center_y_coordinate", "Elevation", "Canton", "Municipality")],
                   RP_subset)
RP_subset["Time"] <- "resurvey"
# cut off last 3 letters to later overlap datasets


# difference / delta data
d_subset <- merged_data_subset[ , grep("^d", names(merged_data_subset))]
names(d_subset) <- substr(names(d_subset), 2, nchar(names(d_subset)))  # cut off first letter to later overlap datasets
d_subset <- cbind(merged_data_subset[ , c("PAG", "Precision", "Center_x_coordinate", "Center_y_coordinate", "Elevation", "Canton", "Municipality")],
                  d_subset)
d_subset["Time"] <- "delta"
# rename columns to match the other two datasets
names(d_subset)[names(d_subset) == "Species_richness"] <- "Richness_method_corr"
names(d_subset)[names(d_subset) == "Temprature"] <- "T"
names(d_subset)[names(d_subset) == "Nutrient"] <- "N"
names(d_subset)[names(d_subset) == "Urbanization"] <- "EM" # hemeroby (= urbanization)
names(d_subset)[names(d_subset) == "Moving_tolerance"] <- "MV"
names(d_subset)[names(d_subset) == "Reaction"] <- "R"
names(d_subset)[names(d_subset) == "Moisture"] <- "F"
names(d_subset)[names(d_subset) == "Light"] <- "L"
names(d_subset)[names(d_subset) == "Cover_Cyp_Junc"] <- "Cover_Cyperaceae_Juncaceae"
names(d_subset)[names(d_subset) == "FD_heigh"] <- "FD_height"
names(d_subset)[names(d_subset) == "Cover_forb"] <- "Cover_Forb"


# join all the three datasets
dataset_total <- rbind(HP_subset, RP_subset, d_subset)


library(dplyr)
dataset_long <- bind_rows(HP_subset, RP_subset, d_subset)

##########################################################################################
# transform coordinates from the global system (4326) to swiss system (2056 )
library(sf)

df_swiss <- st_as_sf(dataset_long, coords = c("Center_x_coordinate", "Center_y_coordinate"), crs = 4326) |>
  st_transform(2056)

dataset_long$Center_x_coordinate <- st_coordinates(df_swiss)[,1]
dataset_long$Center_y_coordinate <- st_coordinates(df_swiss)[,2]


write.csv(dataset_long,"C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/Squarefoot code/squarefoot/Squarefoot_data_long.csv", row.names = FALSE)


#split the dataset into the three time options and put them into a sheet of an xlsx file
dataset_historic <- dataset_long[dataset_long$Time == "historic", ]
dataset_resurvey <- dataset_long[dataset_long$Time == "resurvey", ]
dataset_delta <- dataset_long[dataset_long$Time == "delta", ]
library(openxlsx)

write.xlsx(list(
    "historic" = dataset_historic,
    "resurvey" = dataset_resurvey,
    "delta" = dataset_delta),
  file = "squarefoot_data_cleaned.xlsx")

