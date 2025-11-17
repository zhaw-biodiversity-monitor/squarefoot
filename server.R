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

    data_i <- reactive({
      geodata_i() |>
        rename(column_y = input$column_y)
    })

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

  
    })
  })
})
