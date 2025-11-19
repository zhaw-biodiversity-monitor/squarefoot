

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
    "keine Aggregation" = "punkte",
    "Hexagone (10x10km)" = "hex10",
    "Hexagone (20x20km)" = "hex20",
    "Biogeografische Regionen" = "bgr",
    "Kantone" = "kantone"
  ),
  column_options = c(
    "Artenreichtum" = "anzahl_arten",
    "Phylogenetische Diversität (0-1)" = "phylogenetische_diversita0et",
    "Funktionale Diversität (0-1)" = "funktionale_diversita0et",
    "Funktionale Diversität spezifische Blattfläche (0-1)" = "funktionale_diversita0et_spezifische_blattfla0eche",
    "Funktionale Diversität Samengewicht (0-1)" = "funktionale_diversita0et_samengewicht",
    "Funktionale Diversität Höhe (0-1)" = "funktionale_diversita0et_ho0ehe",
    "Mittlere Temperatur (1–5)" = "temperatur",
    "Mittlerer Nährstoff (1–5)" = "na0ehrstoff",    
    "Mittlere Reaktion (1–5)" = "reaktion",    
    "Mittlere Feuchtigkeit (1–5)" = "feuchtigkeit",
    "Mittlere Lichtzahl (1–5)" = "licht",    
    "Mittlere Mahdverträglichkeit (1–5)" = "mahdvertra0eglichkeit",
    "Mittlere Hemerobie (1–5)" = "hemerobiezahl",
    "Mittlere Stresszahl (0–1)" = "stresszahl",
    "Mittlere Konkurrenzstrategie (0–1)" = "konkurrenzstrategie",
    "Mittlere Ruderalstrategie (0–1)" = "ruderalstrategie",
    "Anteil Deckung Poaceae" = "anteil_deckung_poaceae",
    "Anteil Deckung Krautartige" = "anteil_deckung_krautartige",
    "Anteil Deckung  Cyperaceae & Juncaceae" = "anteil_deckung_cyperaceae_und_juncaceae"
  ),

  # column_options = c(
  #   "Artenreichtum" = "species_richness",
  #   "Mittleren Temperaturzahl (x–y)" = "temperature",
  #   "Mittlere Lichtzahl (x–y)" = "light",
  #   "Mittlerer Feuchtezahl (x–y)" = "moisture",
  #   "Mittlerer Reaktionszahl (x–y)" = "reaction",
  #   "Mittlerer Nährstoffzahl (x–y)" = "nutrient",
  #   "Mittlerer Konkurrenzstrategie (0–1)" = "csr_competitive_ability",
  #   "Mittlerer Ruderalstrategie (0–1)" = "csr_disturbance_tolerance",
  #   "Mittlerer Stresszahl (0–3)" = "csr_stress_tolerance",
  #   "Mittlerer Mahdverträglichkeitszahl (1–5)" = "moving_tolerance",
  #   "Phylogenetische Diversität" = "phylogenetic_diversity",
  #   "Funktionale Diversität" = "functional_diversity",
  #   "Funktionale Diversität spezifische Blattfläche" = "funct_div_spec_leaf_area",
  #   "Funktionale Diversität Samengewicht" = "funct_div_seed_mass",
  #   "Funktionale Diversität Höhe" = "funct_div_height",
  #   "Mittlere Hemerobiezahl (1-5)" = "urbanization",
  #   "Anteil Deckung Poaceae" = "cover_poaceae",
  #   "Anteil Deckung Krautartige" = "cover_forb",
  #   "Anteil Deckung  Cyperaceae & Juncaceae" = "cover_cyp_junc"
  # ),

  
  time_aspect = c("historisch (Jahre 1884 - 1931)" = "historisch",
                  "resurvey (Jahre 2021, 2022)" = "resurvey", 
                  "Differenz" = "delta")
  #time = c("historic", "resurvey", "delta")
)

# Color Configuration
COLOR_CONFIG <- list(
  bivariate_palette = RColorBrewer::brewer.pal(3, "RdYlBu")
  # library(scales)
  # show_col(bivariate_palette)
) 
COLOR_CONFIG_5 <- list(
  bivariate_palette = RColorBrewer::brewer.pal(5, "RdYlBu")
  # library(scales)
  # show_col(bivariate_palette)
)
# Color definitions
mycols <- list(
  drawing = list(
    rgba_string = "rgba(0, 51, 255, 1)",
    hex = "#0033FF"
  ),
  selected_polygon = list(
    rgba_string = "rgba(255, 48, 0, 1)",
    hex = "#ff3000"
  )
)