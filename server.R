source("libraries.R")
source("config.R")
source("utils.R")
source("map_module.R")
source("data_module.R")

#setwd("C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/")

library(magrittr)
library(RColorBrewer)
library(colorspace)
#rsconnect::setAccountInfo(name='yahe55', token='AB31184339DED9A0C60A37741C8527F9', secret='hUBnmpjMhA+M7VvbgqdQsaQmWeYvIec80jRQ8Yox')
#library(rsconnect)
#rsconnect::deployApp('C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/')

# Load initial data
geodata <- load_geodata()

#######################################################################

shinyServer(function(input, output) {
  # Initialize map
  output$map <- renderLeaflet({
    init_map()
  })
  geodata_i <- reactive({
    select_dataset(geodata,input$aggregation, input$time_a)
    #browser()
    # take new structure of data and try to get select dataset to work
  })

  # # filter for one of the three datasets: historic, delta and resurvey
  # geodata_t <- reactive({
  #    t <- input$time_a
  #    geodata[[t]]
  # })
  #
  # # get the aggregation data
  # geodata_i <- reactive({
  #   data <- geodata_t()
  #   data[[input$aggregation]]
  # })

  # Observe changes and update map
  observe({
    geodata_i <- geodata_i()
    #browser()

    if (input$aggregation == "punkte") {

      # Get column values
      ycol <- get_column_values(geodata_i, input$column_y)
      print(input$time_a) # make condition that says it contains delta and ignores the rest

      # Update map with points, different thresholds for delta and historic/resurvey
      if (input$time_a == "delta") {leafletProxy("map") |>
        update_map_points(geodata_i, ycol, input$column_y, threshold_delta, input$time_a)
      } else {leafletProxy("map") |>
        update_map_points(geodata_i, ycol, input$column_y, threshold_res_hist, input$time_a)
      }
        } else {
      # Get column values
      ycol <- get_column_values(geodata_i, input$column_y)
      n_obs <- geodata_i$n

      # Update map with polygons
      if ( input$time_a == "delta"){
        leafletProxy("map") |>
          clearMarkers() |>
          update_map_polygons(geodata_i, ycol, n_obs, input$column_y, threshold=threshold_delta, time_aspect = input$time_a)
      } else{
      leafletProxy("map") |>
        clearMarkers() |>
        update_map_polygons(geodata_i, ycol, n_obs, input$column_y, threshold=threshold_res_hist, time_aspect = input$time_a)
        }
      }

    ranges <- reactive({
      browse()
      all_features <- input$map_draw_all_features
      features <- all_features$features
      coords <- map(features, \(x)x$geometry$coordinates[[1]])
      map(coords, \(x) {
        x |>
          map(\(y)c(y[[1]], y[[2]])) |>
          do.call(rbind, args = _) |>
          apply(2, range)
      })
    })
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
    # selected_object <- reactiveVal("") ###################################################################################
    # observeEvent(input$map_shape_click, {
    #   loc_list <- input$map_shape_click
    #   # print("loc_list:")
    #   # print(loc_list)
    #   # print("------------------")
    #   geodata_i <- select_dataset(geodata, input$aggregation, input$time_a)
    #   # print("geodata_i:")
    #   # print(geodata_i)
    #   # print("------------------")
    #   loc <- st_point(c(loc_list$lng, loc_list$lat)) |>
    #     st_sfc(crs = 4326)
    #   # print("loc:")
    #   # print(loc)
    #   # print("------------------")
    #   selected_object_str <- as.vector(geodata_i[loc, input$aggregation, drop = TRUE])
    #   # print("selected_object_str:")
    #   # print(selected_object_str)
    #   # print("------------------")
    #   selected_object(selected_object_str)
    #   print(selected_object_str)
    # })

    output$scatterplot <- renderPlotly({
      
      data <- na.omit(data.frame(altitude=data_i()$altitude, column_y=data_i()$column_y))

      if (input$aggregation == "punkte") {
        #browser()
        if (input$time_a =="delta"){
          data <- add_color_scale(threshold_delta, input$time_a, input$column_y, data, "light")
        } else{
          data <- add_color_scale(threshold_res_hist, input$time_a,input$column_y, data, "light")
        }
        plot_ly(
          data,
          x = ~altitude,
          y = ~column_y,
          type = "scatter",
          mode = "markers",
          marker = list(color = ~plot_color),#list(color = "rgba(255, 182, 193, 1)"),
          name = "alle")|>
          layout(hovermode = TRUE,
              clickmode = "none",
              yaxis = list(title = paste0(clean_names(input$column_y))),
              xaxis = list(title = "Meereshöhe (m.ü.M.)")
             #plot_bgcolor = "darkgrey"
             # modebar = get_modebar_config()
            )
      }
      else { 
        #get colors for aggregated points
        if (input$time_a =="delta"){
          data <- add_color_scale(threshold_delta, input$time_a,input$column_y, data, "dark")
        } else{
          data <- add_color_scale(threshold_res_hist, input$time_a,input$column_y, data, "dark")
        }
       # get data and colors from not aggregated data additionally
        ycol <- get_column_values(select_dataset(geodata,"punkte", input$time_a), input$column_y)
        data_t <- na.omit(data.frame(altitude=select_dataset(geodata,"punkte", input$time_a)$altitude, column_y=ycol))
        if (input$time_a =="delta"){
          data_t <- add_color_scale(threshold_delta, input$time_a,input$column_y, data_t, "light")
        } else{
          data_t <- add_color_scale(threshold_res_hist, input$time_a,input$column_y, data_t, "light")
        }

        plot_ly(
          data_t,
          x = ~altitude,
          y = ~column_y,
          type = "scatter",
          mode = "markers",
          marker = list(color = ~plot_color),#list(color = "rgba(255, 182, 193, 1)"),
          name = "alle")|>
          layout(hovermode = TRUE,
                 clickmode = "none",
                 yaxis = list(title = paste0(clean_names(input$column_y))),
                 xaxis = list(title = "Meereshöhe (m.ü.M.)")
                 #plot_bgcolor = "darkgrey"
                 # modebar = get_modebar_config()
          )|>
          add_markers(data = data,
                      x= ~altitude,
                      y=~column_y,
                      type="scatter",
                      mode="markers",
                      marker=list(color = ~plot_color),
                      name="aggregation")

      }

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

