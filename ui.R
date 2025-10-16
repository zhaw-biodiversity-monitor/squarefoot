source("libraries_sqft.R")
source("config_sqft.R")
source("utils_sqft.R")

gpkg_path <- "vectors_test.gpkg"
# "C:/Users/hedd/Documents/Dashboard VegChange Projekt/Squarefoot/grassland-data-main/appdata/vectors_resurvey.gpkg"
# layers <- read_sf(gpkg_path, "layers_overview")

layers <- st_layers(gpkg_path)$name
# 
# layers <- read_sf(gpkg_path, "layers_overview")
# aggregation1 <- unique(layers$aggregation1)
# aggregation1 <- aggregation1[aggregation1 != "layers"]
# 



aggregation1 <- c(
  "keine Aggregation" = "punkte",
  "Hexagone (10x10km)" = "hex10",
  "Hexagone (20x20km)" = "hex20",
  "Biogeografische Regionen" = "bgr",
  "Kantone"="kantone"
  )

# aggregation1 <- aggregation1[aggregation1 != "layers"]

# aggregation1 <- c("hex10","hex20","BGR","kantone")
# datasets <- c("normallandschaft") # ,"tww","moore"


dataset_options = c(
  "delta" = 1,
  "historisch" = 2,
  "resurvey" = 3
)

lebensraumgruppen <- c("Grasland","Zwergstrauchheide","Wald")





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
      
      conditionalPanel(
        condition = "input.aggregation == 'punkte'",
        shinyWidgets::pickerInput(
          "dataset",
          "Datenset",
          choices = UI_CONFIG$dataset_options,
          selected = UI_CONFIG$dataset_options,
          options = pickerOptions(actionsBox = TRUE),
          multiple = TRUE
        ),
        
        shinyWidgets::pickerInput(
          "lebensraumgruppen",
          "Lebensraumgruppen",
          choices = UI_CONFIG$habitat_groups,
          selected = UI_CONFIG$habitat_groups,
          multiple = TRUE
        ),

        sliderInput(
          "flaeche",
          "Plotgrösse",
          min = 0,
          max = 500,
          step = 50,
          value = c(0,500)
        ),
        plotly::plotlyOutput("scatterplot")
      ),
      
      
    ),
    
    mainPanel(
      leaflet::leafletOutput("map", height = 600)
    )
  )
))
