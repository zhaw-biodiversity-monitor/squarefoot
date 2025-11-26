#########################################################################################
# define various configurations


# Map Configuration
MAP_CONFIG <- list(
  bounds = list(
    west = 5.955902,
    south = 45.81796,
    east = 10.49206,
    north = 47.80845
  ),
  tile_layers = list(
    grau = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-grau/default/current/3857/{z}/{x}/{y}.jpeg",
    swissimage = "https://wmts.geo.admin.ch/1.0.0/ch.swisstopo.swissimage/default/current/3857/{z}/{x}/{y}.jpeg",
    farbig = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg"
  )
)

# Data Configuration
DATA_CONFIG <- list(
  gpkg_path = "appdata/vectors_squarefoot.gpkg",
  csv_path = "appdata/squarefoot.csv"
)

# UI Configuration
UI_CONFIG <- list(
  aggregation_options = c(
    "no aggregation" = "punkte",
    "Hexagons (10x10km)" = "hex10",
    "Hexagons (20x20km)" = "hex20",
    "biogeographical regions" = "bgr",
    "cantons" = "kantone"
  ),
  column_options = c(
    "Species Richness" = "species_richness",
    "Phylogenetic Diversity (0-1)" = "phylogenetic_diversity",
    "Functional Diversity (0-1)" = "functional_diversity",
    "Functional Diversity Specific Leaf Area (0-1)" = "functional_diversity_specific_leaf_area",
    "Functional Diversity Seed Mass (0-1)" = "functional_diversity_seed_mass",
    "Functional Diversity Height (0-1)" = "functional_diversity_height",
    "Mean Temperature (1–5)" = "temperature",
    "Mean Nutrient (1–5)" = "nutrient",    
    "Mean Reaction (1–5)" = "reaction",    
    "Mean Moisture (1–5)" = "moisture",
    "Mean Light (1–5)" = "light",    
    "Mean Moving Tolerance (1–5)" = "moving_tolerance",
    "Mean Hemeroby (1–5)" = "hemeroby",
    "Mean Stress Tolerance (0–1)" = "stress_tolerance",
    "Mean Competitive Ability (0–1)" = "competitive_ability",
    "Mean Disturbance Tolerance (0–1)" = "disturbance_tolerance",
    "Cover Poaceae" = "cover_poaceae",
    "Cover Forb" = "cover_forb",
    "Cover Cyperaceae & Juncaceae" = "cover_cyperaceae_and_juncaceae"
  ),

  time_aspect = c("historic (years 1884 - 1931)" = "historisch",
                  "resurvey (years 2021, 2022)" = "resurvey", 
                  "difference" = "delta")
)

# Color Configuration
COLOR_CONFIG <- list(
  bivariate_palette = RColorBrewer::brewer.pal(3, "RdYlBu")
) 
