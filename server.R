source("libraries.R")
source("config.R")
source("utils.R")
source("map_module.R")
source("data_module.R")

setwd("C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/")
library(magrittr)


# Load initial data
geodata <- load_geodata()

#######################################################################

shinyServer(function(input, output) {
  # Initialize map
  output$map <- renderLeaflet({
    init_map()
  })
  
  # filter for one of the three datasets: historic, delta and resurvey
  geodata_t <- reactive({
     t <- input$time_a
     geodata[[t]]
  })
  
  # get the aggregation data
  geodata_i <- reactive({ 
    data <- geodata_t()        
    data[[input$aggregation]]
  })
  
  # Observe changes and update map
  observe({
    geodata_i <- geodata_i()
    
    if (input$aggregation == "punkte") {
      # n_obs <- geodata_i$n      
      # Filter data for points
      # filtered_data <- filter_data(
      #   geodata_i,
      #   input$time_a
      # )
      
      # Get column values
      ycol <- get_column_values(geodata_i, input$column_y)
      
      # Update map with points
      leafletProxy("map") |>
        update_map_points(geodata_i, ycol, input$column_y)#, n_obs)
    } else {
      # Get column values
      ycol <- get_column_values(geodata_i, input$column_y)
      n_obs <- geodata_i$n
      
      # Update map with polygons
      leafletProxy("map") |>
        clearMarkers() |> 
        update_map_polygons(geodata_i, ycol, n_obs, input$column_y)
    }
    # 
    # ranges <- reactive({
    #   all_features <- input$map_draw_all_features
    #   features <- all_features$features
    #   coords <- map(features, \(x)x$geometry$coordinates[[1]])
    #   map(coords, \(x) {
    #     x |>
    #       map(\(y)c(y[[1]], y[[2]])) |>
    #       do.call(rbind, args = _) |>
    #       apply(2, range)
    #   })
    # })
    # sqft_inbounds <- reactive({
    #   if (length(ranges()) > 0) {
    #     ranges <- ranges()[[1]]
    #     lat <- ranges[, 2]
    #     lng <- ranges[, 1]
    #     geodata_i() |>
    #       filter(
    #         lange > min(lng),
    #         lange < max(lng),
    #         breite > min(lat),
    #         breite < max(lat)
    #       )
    #   } else {
    #     geodata_i()[FALSE, ]
    #   }
    # })
    # sqft_renamed <- reactive({
    #   geodata_i() |>
    #     rename(column_y = input$column_y) #|>
    #     #rename(agg = input$aggregation)
    # })
    # sqft_inbounds_renamed <- reactive({
    #   sqft_inbounds() |>
    #     rename(column_y = input$column_y)# |>
    #     #rename(agg = input$aggregation)
    # })

    data_i <- reactive({
      geodata_i() |>
        rename(column_y = input$column_y)
    })
    output$scatterplot <- renderPlotly({
      #browser()
      data <- na.omit(data.frame(altitude=data_i()$altitude, column_y=data_i()$column_y))
      plot_ly(
        data,
        x = ~altitude,
        y = ~column_y,
        type = "scatter",
        mode = "markers",
        marker = list(color = "rgba(255, 182, 193, 1)"),
        name = "alle")|>
        layout(hovermode = TRUE, 
            clickmode = "none",
            yaxis = list(title = paste0(clean_names(input$column_y))),
            xaxis = list(title = "Meereshöhe (m.ü.M.)")
           # modebar = get_modebar_config()
          )

      # fig <- create_base_scatter(sqft_renamed(), "meereshohe", "column_y") |>
      #   add_trace(
      #     data = sqft_inbounds_renamed(),
      #     color = "",
      #     marker = list(
      #       color = "rgba(255,255,255,0)",
      #       line = list(color = mycols$drawing$rgba_string, width = 2)
      #     ),
      #     name = "in der Auswahl"
      #   )
      
      # if (selected_object() != "") {
      #   grassland_inpolygon <- grassland_renamed()[grassland_renamed()$agg == selected_object(), ]
      #   fig <- fig |>
      #     add_trace(
      #       data = grassland_inpolygon,
      #       color = "",
      #       marker = list(
      #         color = "rgba(255,255,255,0)",
      #         line = list(color = mycols$selected_polygon$rgba_string, width = 2)
      #       ),
      #       name = "in polygon"
      #     )
      # }
      
      # fig |>
      #   layout(
      #     hovermode = FALSE,
      #     clickmode = "none",
      #     yaxis = list(title = paste0(clean_names(input$column_y))),
      #     xaxis = list(title = "Meereshöhe (m ü.M.)"),
      #    # modebar = get_modebar_config()
      #   )
    })
  })
})






# observe({
#   geodata_i <- geodata_i()
#   geodata_i <- geodata_i[!is.na(geodata_i$altitude), ]
#   
#   ycol <- get_column_values(geodata_i, input$column_y)
#   #ycol <- input$column_y
#   output$scatterplot <- renderPlotly({
#     plot_ly(
#       data = geodata_i,
#       x = ~altitude,
#       y = ycol, #label hier verdoppelt worden ?
#       type = "scatter",
#       mode="markers",
#       name= "alle Punkte"
#     )|>
#       layout(yaxis = list(
#         title = clean_names(input$column_y),
#         showticklabels = TRUE
#         #range = c(-5, 5),
#         #autorange = F, # TODO: beim anzeigen stimmt die range der y achse nicht - sowas wie ylim aber hier?
#         # - kann nachher die information überschireben werden sodass meine änderungen hier nicht wirken?
#         #scaleanchor = F #NULL?
#         #scaleratio=10
#         #rangemode="normal"
#         #anchor="free"
#       ),
#       xaxis = list(
#         title = "Meereshöhe (M.ü.M.)",
#         showticklabels = TRUE
#         #autorange = T,
#         #range = c(100,2900),
#         #scaleanchor = F
#       )
#       )
#   })
# })
