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
data_1 <- read.csv("raw data/SqFt_HP_env.csv", sep =";") 
data_2 <- read.csv("raw data/PAG_dDiv.csv", sep=";")

names(data_1)
# length(data_1$PAG)
# length(data_2$PAG)
# length(unique(data_1$PAG))
# length(unique(data_2$PAG))

# merge the datasets by unique identifiers (PAG)
data_1 <- data_1[c("PAG", "Precision", "Center_x_coordinate", "Center_y_coordinate", "Canton", "Municipality", "Altitude_original")]
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

write.csv(merged_data_subset, "one-off scripts/Squarefoot_data.csv", row.names = FALSE)


###########################################################################################
# transform data time format wide to long - where the time variable is categorical
colnames(merged_data_subset)

###########################################################################################
# historic data is marked with _HP at the end of the variable name
HP_subset <- merged_data_subset[ , grepl("_HP$", names(merged_data_subset))]
names(HP_subset) <- substr(names(HP_subset), 1, nchar(names(HP_subset)) - 3) # cut off last 3 letters to later overlap datasets
HP_subset <- cbind(merged_data_subset[ , c("PAG", "Precision", "Center_x_coordinate", "Center_y_coordinate", "Elevation", "Canton", "Municipality", "Altitude_original")],
                   HP_subset)
HP_subset["Time"] <- "historic"

# rename variables so all 3 datasets match
names(HP_subset)[names(HP_subset) == "Richness_method_corr"] <- "Species_richness"
names(HP_subset)[names(HP_subset) == "T"] <- "Temperature"
names(HP_subset)[names(HP_subset) == "N"] <- "Nutrient"
names(HP_subset)[names(HP_subset) == "EM"] <- "Urbanization" # hemeroby (= urbanization)
names(HP_subset)[names(HP_subset) == "MV"] <- "Moving_tolerance"
names(HP_subset)[names(HP_subset) == "R"] <- "Reaction"
names(HP_subset)[names(HP_subset) == "F"] <- "Moisture"
names(HP_subset)[names(HP_subset) == "L"] <- "Light"
names(HP_subset)[names(HP_subset) == "Cover_Cyperaceae_Juncaceae"] <- "Cover_Cyp_Junc"


###########################################################################################
# resurvey data, marked with _RP at the end of the variable name
RP_subset <- merged_data_subset[ , grepl("_RP$", names(merged_data_subset))]
names(RP_subset) <- substr(names(RP_subset), 1, nchar(names(RP_subset)) - 3) # cut off last 3 letters to later overlap datasets
RP_subset <- cbind(merged_data_subset[ , c("PAG", "Precision", "Center_x_coordinate", "Center_y_coordinate", "Elevation", "Canton", "Municipality", "Altitude_original")],
                   RP_subset)
RP_subset["Time"] <- "resurvey"

#rename to match
names(RP_subset)[names(RP_subset) == "Richness_method_corr"] <- "Species_richness"
names(RP_subset)[names(RP_subset) == "T"] <- "Temperature"
names(RP_subset)[names(RP_subset) == "N"] <- "Nutrient"
names(RP_subset)[names(RP_subset) == "EM"] <- "Urbanization" # hemeroby (= urbanization)
names(RP_subset)[names(RP_subset) == "MV"] <- "Moving_tolerance"
names(RP_subset)[names(RP_subset) == "R"] <- "Reaction"
names(RP_subset)[names(RP_subset) == "F"] <- "Moisture"
names(RP_subset)[names(RP_subset) == "L"] <- "Light"
names(RP_subset)[names(RP_subset) == "Cover_Cyperaceae_Juncaceae"] <- "Cover_Cyp_Junc"


###########################################################################################
# difference of historic and resurvey -> delta data, marked with a d at the beginning of the variable name
d_subset <- merged_data_subset[ , grep("^d", names(merged_data_subset))]
names(d_subset) <- substr(names(d_subset), 2, nchar(names(d_subset)))  # cut off first letter to later overlap datasets
d_subset <- cbind(merged_data_subset[ , c("PAG", "Precision", "Center_x_coordinate", "Center_y_coordinate", "Elevation", "Canton", "Municipality", "Altitude_original")],
                  d_subset)
d_subset["Time"] <- "delta"

# rename columns to match the other two datasets
names(d_subset)[names(d_subset) == "FD_heigh"] <- "FD_height"
names(d_subset)[names(d_subset) == "Temprature"] <- "Temperature"
names(d_subset)[names(d_subset) == "Cover_forb"] <- "Cover_Forb"


###########################################################################################
# join all the three datasets
#dataset_total <- rbind(HP_subset, RP_subset, d_subset)
library(dplyr)
dataset_long <- bind_rows(HP_subset, RP_subset, d_subset)

names(dataset_long)[names(dataset_long) == "PD"] <- "Phylogenetic_diversity"
names(dataset_long)[names(dataset_long) == "FD"] <- "Functional_diversity"
names(dataset_long)[names(dataset_long) == "FD_sla"] <- "Functional_diversity_specific_leaf_area"
names(dataset_long)[names(dataset_long) == "FD_seed_mass"] <- "Functional_diversity_seed_mass"
names(dataset_long)[names(dataset_long) == "FD_height"] <- "Functional_diversity_height"
names(dataset_long)[names(dataset_long) == "Altitude_original"] <- "Altitude"
names(dataset_long)[names(dataset_long) == "Urbanization"] <- "EIV_Hemeroby"

##########################################################################################
# transform coordinates from the global system (4326) to swiss system (2056 )
library(sf)

df_swiss <- st_as_sf(dataset_long, coords = c("Center_x_coordinate", "Center_y_coordinate"), crs = 4326) |>
  st_transform(2056)

dataset_long$Center_x_coordinate <- st_coordinates(df_swiss)[,1]
dataset_long$Center_y_coordinate <- st_coordinates(df_swiss)[,2]

###########################################################################################
# save in english
names(dataset_long)
names(dataset_long)[names(dataset_long) == "EIV_Hemeroby"] <- "Hemeroby"
names(dataset_long)[names(dataset_long) == "Cover_Cyp_Junc"] <- "Cover_Cyperaceae_and_Juncaceae"
names(dataset_long)[names(dataset_long) == "CSR_Stress_tolerance"] <- "Stress_tolerance"
names(dataset_long)[names(dataset_long) == "CSR_Disturbance_tolerance"] <- "Disturbance_tolerance"
names(dataset_long)[names(dataset_long) == "CSR_Competitive_ability"] <- "Competitive_ability"

#write.csv(dataset_long,"one-off scripts/Squarefoot_data_long_eng.csv", row.names = FALSE)



###########################################################################################

#translate the variables to display to german
names(dataset_long)[names(dataset_long) == "Species_richness"] <- "Anzahl_Arten" 
names(dataset_long)[names(dataset_long) == "Phylogenetic_diversity"] <- "Phylogenetische_Diversität"
names(dataset_long)[names(dataset_long) == "Functional_diversity"] <- "Funktionale_Diversität"
names(dataset_long)[names(dataset_long) == "Functional_diversity_specific_leaf_area"] <- "Funktionale_Diversität_spezifische_Blattfläche"
names(dataset_long)[names(dataset_long) == "Functional_diversity_seed_mass"] <- "Funktionale_Diversität_Samengewicht"
names(dataset_long)[names(dataset_long) == "Functional_diversity_height"] <- "Funktionale_Diversität_Höhe"
names(dataset_long)[names(dataset_long) == "Temperature"] <- "Temperaturzahl"
names(dataset_long)[names(dataset_long) == "Nutrient"] <- "Nährstoffzahl"
names(dataset_long)[names(dataset_long) == "Reaction"] <- "Reaktionszahl"
names(dataset_long)[names(dataset_long) == "Moisture"] <- "Feuchtigkeitszahl"
names(dataset_long)[names(dataset_long) == "Light"] <- "Licht"
names(dataset_long)[names(dataset_long) == "Moving_tolerance"] <- "Mahdverträglichkeit"
names(dataset_long)[names(dataset_long) == "Hemeroby"] <- "Hemerobiezahl"
names(dataset_long)[names(dataset_long) == "Cover_Poaceae"] <- "Anteil_Deckung_Poaceae"
names(dataset_long)[names(dataset_long) == "Cover_Forb"] <- "Anteil_Deckung_Krautartige"
names(dataset_long)[names(dataset_long) == "Cover_Cyperaceae_and_Juncaceae"] <- "Anteil_Deckung_Cyperaceae_und_Juncaceae"
names(dataset_long)[names(dataset_long) == "Stress_tolerance"] <- "Stresszahl"
names(dataset_long)[names(dataset_long) == "Disturbance_tolerance"] <- "Ruderalstrategie"
names(dataset_long)[names(dataset_long) == "Competitive_ability"] <- "Konkurrenzstrategie"


# save in german
write.csv(dataset_long,"one-off scripts/Squarefoot_data_long_ger.csv", row.names = FALSE)




