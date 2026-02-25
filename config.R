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
    "Mittlere Temperaturzahl (1–5)" = "temperaturzahl",
    "Mittlere Nährstoffzahl (1–5)" = "na0ehrstoffzahl",    
    "Mittlere Reaktionszahl (1–5)" = "reaktionszahl",    
    "Mittlere Feuchtigkeitszahl (1–5)" = "feuchtigkeitszahl",
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

  time_aspect = c("historisch (Jahre 1884 - 1931)" = "historisch",
                  "resurvey (Jahre 2021, 2022)" = "resurvey", 
                  "Differenz" = "delta")
)

# Color Configuration
COLOR_CONFIG <- list(
  bivariate_palette = RColorBrewer::brewer.pal(3, "RdYlBu")
) 
