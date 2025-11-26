#########################################################################################
# load packages in libraries.R and other important files or data
source("libraries.R")
source("config.R")
source("utils.R")
source("map_module.R")
source("data_module.R")

#setwd("./squarefoot/") # set working directory in squarefoot folder, with the shiny files in it, and the loaded data in an appdata folder

# library(magrittr)
# library(RColorBrewer)

# set up publication in shiny.io server
#rsconnect::setAccountInfo(name='yahe55', token='AB31184339DED9A0C60A37741C8527F9', secret='hUBnmpjMhA+M7VvbgqdQsaQmWeYvIec80jRQ8Yox')
#library(rsconnect)
#rsconnect::deployApp('C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot/')

# Load initial data from .gpkg file
geodata <- load_geodata()

#########################################################################################

shinyServer(function(input, output) {
  # Initialize map
  output$map <- renderLeaflet({
    init_map()
  })
  
  # get one of the datasets depending on the time aspect input and the aggregation input
  geodata_i <- reactive({
    select_dataset(geodata,input$aggregation, input$time_a)
  })

  # # filter for one of the three datasets: historic, delta and resurvey - in this case, load_geodata_2 needs to be used
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

    if (input$aggregation == "punkte") { # if there is no aggregation selected

      # Get column values from the chosen variable to display
      ycol <- get_column_values(geodata_i, input$column_y)

      # Update map with points, use different thresholds for delta and historic/resurvey
      if (input$time_a == "delta") {
        leafletProxy("map") |>
        update_map_points(geodata_i, ycol, input$column_y, threshold_delta, input$time_a)
      } 
      else { #historic/resurvey
        leafletProxy("map") |>
        update_map_points(geodata_i, ycol, input$column_y, threshold_res_hist, input$time_a)
      }
    } 
    else { # if any aggregation is selected (hex10, hex10, bgr or kantone)
          
      # Get column values from the chosen variable to display
      ycol <- get_column_values(geodata_i, input$column_y)
      n_obs <- geodata_i$n # get ovbservation numbers for the aggregations for color coding and to display in the popup

      # Update map with polygons,  use different thresholds for delta and historic/resurvey
      if ( input$time_a == "delta"){
        leafletProxy("map") |>
          clearMarkers() |>
          update_map_polygons(geodata_i, ycol, n_obs, input$column_y, threshold=threshold_delta, time_aspect = input$time_a)
      } 
      else{ #historic/resurvey
      leafletProxy("map") |>
        clearMarkers() |>
        update_map_polygons(geodata_i, ycol, n_obs, input$column_y, threshold=threshold_res_hist, time_aspect = input$time_a)
      }
    }
    
    # add a scatterplot with different colors according to the thresholds
    # get the data from the chosen variable to display
    data_i <- reactive({
      geodata_i() |>
        rename(column_y = input$column_y)
    })
    
    # construct the scatterplot
    output$scatterplot <- renderPlotly({
      
      # change data format to dataframe because the plot_ly struggles with recognising it correctly (ranges and the legend are the problem)
      data <- na.omit(data.frame(altitude=data_i()$altitude, column_y=data_i()$column_y))
      
      # if no aggregation is selected: just display the individual points
      if (input$aggregation == "punkte") {
        # use different thresholds for delta and resurvey/historic, get back data with an additional row called plot_color
        if (input$time_a =="delta"){
          data <- add_color_scale(threshold_delta, input$time_a, input$column_y, data, "light")
        } 
        else { #historic/resurvey
          data <- add_color_scale(threshold_res_hist, input$time_a,input$column_y, data, "light")
        }
        
        plot_ly(
          data,
          x = ~altitude,
          y = ~column_y, #chosen variable to be displayed
          type = "scatter",
          mode = "markers",
          marker = list(color = ~plot_color))|> # colors according to thresholds
          layout(hovermode = TRUE,
              clickmode = "none",
              yaxis = list(title = paste0(clean_names(input$column_y))),
              xaxis = list(title = "Altitude (m a.s.l.)") #alternatively call it elevation?
              #plot_bgcolor = "darkgrey" # trying to adjust background color because the yellow points are not well visible - I changed the yellow to be darker in map_module/add_color_scale
            )
      }
      # if any aggregation is selected: display the individual points (lightened)  and add the aggregation as darker points
      # data here is the aggregated polygons
      else { 
        #get colors for aggregated points
        #use different thresholds for delta and resurvey/historic, get back data with an additional row called plot_color
        if (input$time_a =="delta"){
          data <- add_color_scale(threshold_delta, input$time_a,input$column_y, data, "dark")
        } 
        else{ #historic/resurvey
          data <- add_color_scale(threshold_res_hist, input$time_a,input$column_y, data, "dark")
        }
        
        # get data and colors from not aggregated data additionally
        ycol <- get_column_values(select_dataset(geodata,"punkte", input$time_a), input$column_y)
        data_t <- na.omit(data.frame(altitude=select_dataset(geodata,"punkte", input$time_a)$altitude, column_y=ycol))
        if (input$time_a =="delta"){
          data_t <- add_color_scale(threshold_delta, input$time_a,input$column_y, data_t, "light")
        } 
        else{ #historic/resurvey
          data_t <- add_color_scale(threshold_res_hist, input$time_a,input$column_y, data_t, "light")
        }

        plot_ly( # plot not aggregated points
          data_t,
          x = ~altitude,
          y = ~column_y, #chosen variable to be displayed
          type = "scatter",
          mode = "markers",
          marker = list(color = ~plot_color),  # colors according to thresholds
          name = "all")|>
          layout(hovermode = TRUE,
                 clickmode = "none",
                 yaxis = list(title = paste0(clean_names(input$column_y))),
                 xaxis = list(title = "Altitude (m a.s.l.)")#alternatively call it elevation?
                 #plot_bgcolor = "darkgrey" # trying to adjust background color because the yellow points are not well visible - I changed the yellow to be darker in map_module/add_color_scale
          )|>
          add_markers(data = data, # add aggregated datapoints
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
