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
  gpkg_path = "vectors_resurvey_test.gpkg",
  csv_path = "resurvey_test.csv"
)

# UI Configuration
UI_CONFIG <- list(
  aggregation_options = c(
    "keine Aggregation" = "punkte",
    "Hexagone (10x10km)" = "hex10",
    "Hexagone (20x20km)" = "hex20",
    "Biogeografische Regionen" = "bgr",
    "Kantone" = "kantone"
  ),
  
  column_options = c(
    "Artenreichtum" = "species_richness",
    "Mittleren Temperaturzahl (x–y)" = "temperature",
    "Mittlere Lichtzahl (x–y)" = "light",
    "Mittlerer Feuchtezahl (x–y)" = "moisture",
    "Mittlerer Reaktionszahl (x–y)" = "reaction",
    "Mittlerer Nährstoffzahl (x–y)" = "nutrient",
    "Mittlerer Konkurrenzstrategie (0–1)" = "csr_competitive_ability",
    "Mittlerer Ruderalstrategie (0–1)" = "csr_disturbance_tolerance",
    "Mittlerer Stresszahl (0–3)" = "csr_stress_tolerance",
    "Mittlerer Mahdverträglichkeitszahl (1–5)" = "moving_tolerance",
    "Phylogenetische Diversität" = "phylogenetic_diversity",
    "Funktionale Diversität" = "functional_diversity",
    "Funktionale Diversität spezifische Blattfläche" = "funct_div_spec_leaf_area",
    "Funktionale Diversität Samengewicht" = "funct_div_seed_mass",
    "Funktionale Diversität Höhe" = "funct_div_height",
    "Mittlere Hemerobiezahl (1-5)" = "urbanization",
    "Anteil Deckung Poaceae" = "cover_poaceae",
    "Anteil Deckung Krautartige" = "cover_forb",
    "Anteil Deckung  Cyperaceae & Juncaceae" = "cover_cyp_junc"
  ),
  # 
  # dataset_options = c(
  #   "Charmillot et al (2021)" = 1,
  #   "Schindler et al (2022)" = 2,
  #   "Kummli et al (2021)" = 3,
  #   "Hepenstrick et al (2023)" = 4,
  #   "Babbi et al (2023)" = 5,
  #   "Dengler et al (2022)" = 6,
  #   "Kummli et al (2021)" = 7,
  #   "Staubli et al (2021)" = 8
  # ),
  
  time_aspect = c("historic", "resurvey", "delta")
  #time = c("historic", "resurvey", "delta")
)

# Color Configuration
COLOR_CONFIG <- list(
  # drawing = list(
  #   rgba_string = "rgba(0, 51, 255, 1)",
  #   hex = "#0033FF"
  # ),
  # selected_polygon = list(
  #   rgba_string = "rgba(255, 48, 0, 1)",
  #   hex = "#ff3000"
  # ),
  bivariate_palette = RColorBrewer::brewer.pal(3, "RdYlBu")
) 
