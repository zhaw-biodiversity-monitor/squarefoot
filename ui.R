#########################################################################################
# load packages in libraries.R and other important files or data

source("libraries.R")
source("config.R")
source("utils.R")

gpkg_path <- "appdata/vectors_squarefoot.gpkg"
layers <- st_layers(gpkg_path)$name

#########################################################################################

# Define UI for application
shinyUI(fluidPage(
  tags$script(src = "myjs.js"),
  titlePanel("Squarefoot Projekt"), # main title
  
  sidebarLayout(
    sidebarPanel( # left panel
      selectInput( # aggregation panel
        "aggregation", # name used inside the code - input$aggregation used
        "Aggregation", # displayed in app
        UI_CONFIG$aggregation_options # options to choose
      ),
      
      selectInput( # variable to display panel
        "column_y", # name used inside the code - input$column_y used
        "Variable", # displayed in app
        UI_CONFIG$column_options # options to choose
      ),
      
      selectInput( # time aspect panel
          "time_a", # name used inside the code - input$time_a used
          "Zeiteinheit", # displayed in app
          UI_CONFIG$time_aspect # options to choose
      ),
      
      plotly::plotlyOutput("scatterplot")
      
      ),
    
    mainPanel( # right panel with the map of switzerland
      leaflet::leafletOutput("map", height = 600)
    )
  )
))
