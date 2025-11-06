source("libraries.R")
source("config.R")
source("utils.R")

gpkg_path <- "appdata/vectors_squarefoot.gpkg"

# layers <- read_sf(gpkg_path, "layers_overview")

layers <- st_layers(gpkg_path)$name



#lebensraumgruppen <- c("Grasland","Zwergstrauchheide","Wald")
time_a <- c("historisch", "resurvey", "delta")



# sfobs <- st_read(gpkg_path, layers[1])
# stopifnot(all((col_y_options) %in% colnames(sfobs)))




# Define UI for application
shinyUI(fluidPage(
  tags$script(src = "myjs.js"),
  titlePanel("Vegetationsaufnahmen der Schweiz"),
  
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
      
      selectInput( ##########################################
          "time_a",
          "Zeiteinheit",
          UI_CONFIG$time_aspect
          ),
 

        plotly::plotlyOutput("scatterplot")
      ),
    
    mainPanel(
      leaflet::leafletOutput("map", height = 600)
    )
  )
))
