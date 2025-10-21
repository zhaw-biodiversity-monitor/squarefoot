source("libraries.R")
source("config.R")
source("utils.R")

gpkg_path <- "vectors_squarefoot.gpkg"

# layers <- read_sf(gpkg_path, "layers_overview")

layers <- st_layers(gpkg_path)$name






aggregation1 <- aggregation1 <- c(
  "keine Aggregation" = "punkte",
  "Hexagone (10x10km)" = "hex10",
  "Hexagone (20x20km)" = "hex20",
  "Biogeografische Regionen" = "bgr",
  "Kantone"="kantone"
  )

# aggregation1 <- aggregation1[aggregation1 != "layers"]

# aggregation1 <- c("hex10","hex20","BGR","kantone")
# datasets <- c("normallandschaft") # ,"tww","moore"


# datasets2 <- c(
#   "Charmillot et al (2021)" = 1	,
#   "Schindler et al (2022)" = 2	,
#   "Kummli et al (2021)" = 3	,
#   "Hepenstrick et al (2023)" = 4	,
#   "Babbi et al (2023)" = 5	,
#   "Dengler et al (2022)" = 6	,
#   "Kummli et al (2021)" = 7	,
#   "Staubli et al (2021)" = 8	
# )

#lebensraumgruppen <- c("Grasland","Zwergstrauchheide","Wald")
time_a <- c("historic", "resurvey", "delta")






col_y_options <- c(
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
)

# sfobs <- st_read(gpkg_path, layers[1])
# stopifnot(all((col_y_options) %in% colnames(sfobs)))

# sfobs <- st_read(gpkg_path, layers[1])
# stopifnot(all((col_y_options) %in% colnames(sfobs)))



# Define UI for application
shinyUI(fluidPage(
  tags$script(src = "myjs.js"),
  titlePanel("Zeitreihen von Vegetationsaufnahmen der Schweiz"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "aggregation",
        "Aggregation",
        UI_CONFIG$aggregation_options
      ),
      
      selectInput(
        "column_y",
        "Jährlicher Trend von",
        UI_CONFIG$column_options
      ),
      
     shinyWidgets::pickerInput(
          "time_a",
          "Zeiteinheit",
          choices = UI_CONFIG$time_aspect,
          selected = UI_CONFIG$time_aspect,
          multiple = TRUE
          ),
 

        plotly::plotlyOutput("scatterplot")
      ),
    
    mainPanel(
      leaflet::leafletOutput("map", height = 600)
    )
  )
))
