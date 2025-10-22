source("libraries.R")
source("config.R")
source("utils.R")
source("map_module.R")
source("data_module.R")


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

# Load initial data
geodata <- load_geodata()
#dataset_list <- load_dataset_info()

shinyServer(function(input, output) {
  # Initialize map
  output$map <- renderLeaflet({
    init_map()
  })
  
  # Reactive data
  geodata_i <- reactive({
    geodata[[input$aggregation]]
  })
  # geodata_t <- reactive({
  #   geodata[[input$time_a]]
  # })

  # dataset_i <- reactive({
  #   dataset_list
  # })
  
  # Store the currently selected dataset ID
  #selected_dataset_id <- reactiveVal(NULL)
  
  # Create color palette
  # color_palette <- reactive({
  #   if (input$aggregation == "punkte") {
  # 
  #     filtered_data <- filter_data(
  #       geodata_i(),
  #       input$dataset,
  #       input$lebensraumgruppen,
  #       input$flaeche
  #     )
  #     ycol <- get_column_values(filtered_data, input$column_y)
  #     qu <- quantile(ycol, probs = c(0.025, 0.975))
  #     ycol <- pmin(pmax(ycol, qu[1]), qu[2])
  #     
  #     colorNumeric(palette = "RdYlBu", domain = ycol)
  #   }
  # })
  
  
  filtered_data <- reactive({ ######## use filtered data for everything - want the map only for either delta, historic or resurvey, not multiple
    filter_data( #                  scatterplot for all data (where I click it tells the dataset? - no? clicking indicates the aggregation)? or only for the filtered data
      geodata_i(),
      #input$dataset,
      input$time_a
      #input$flaeche
    )
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
    
    
  })
  

  observe({

    if(input$aggregation == "punkte"){
    # browser()
    geodata_i <- geodata_i()

    ycol <- get_column_values(geodata_i, input$column_y)

    maxval <- max(abs(range(ycol)))

    output$scatterplot <- renderPlotly({

      plot_ly(geodata_i,
              x = ~ycol,
              y = ~altitude,
              type = "scatter",
              mode = "markers",
              marker = list(color = "rgba(255, 182, 193, 1)"),
              name = "all" ) |>
        layout(
          xaxis = list(
            title = clean_names(input$column_y),
            showticklabels = TRUE,
            xaxis = list(range = c(-5, 5), autorange = FALSE) # TODO: beim ersten anzeigen stimmt die range der x achse nicht, evtl höhe und variable tauschen?
          ),
          yaxis = list(
            title = "Altitude",
            showticklabels = TRUE,
            autorange = T
          ))
        })
    }
  })# TODO: rest des scatterplot codes zum laufen bringen, wo man die punkte/aggregationen anklicken kann
    #TODO: Warnung: Ignoring 15 observations - nachdem ich den scatterplot hinzugefügt habe
      
     #   add_trace( 
     #     data = grassland_inbounds_renamed(),
     #     color = "",
     #     marker = list(
     #       color = "rgba(255,255,255,0)",
     #       line = list(color = mycols$drawing$rgba_string, width = 2)
     #     ),
     #     name = "in bounds"
     #   )
     # if (selected_object() != "") {
     #   grassland_inpolygon <- grassland_renamed()[grassland_renamed()$agg == selected_object(), ]
     #   
     #   fig <- fig |>
     #     add_trace(data = grassland_inpolygon,
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
    #     yaxis = list(title = paste0(clean_names(input$column_y), add_unit(input$column_y))),
    #     xaxis = list(title = "Jahreszahl"),
    #     modebar = list(
    #       remove = c(
    #         "autoScale2d",
    #         "autoscale",
    #         "editInChartStudio",
    #         "editinchartstudio",
    #         "hoverCompareCartesian",
    #         "hovercompare",
    #         "lasso",
    #         "lasso2d",
    #         "orbitRotation",
    #         "orbitrotation",
    #         "pan",
    #         "pan2d",
    #         "pan3d",
    #         "reset",
    #         "resetCameraDefault3d",
    #         "resetCameraLastSave3d",
    #         "resetGeo",
    #         "resetSankeyGroup",
    #         "resetScale2d",
    #         "resetViewMapbox",
    #         "resetViews",
    #         "resetcameradefault",
    #         "resetcameralastsave",
    #         "resetsankeygroup",
    #         "resetscale",
    #         "resetview",
    #         "resetviews",
    #         "select",
    #         "select2d",
    #         "sendDataToCloud",
    #         "senddatatocloud",
    #         "tableRotation",
    #         "tablerotation",
    #         "toImage",
    #         "toggleHover",
    #         "toggleSpikelines",
    #         "togglehover",
    #         "togglespikelines",
    #         "toimage",
    #         "zoom",
    #         "zoom2d",
    #         "zoom3d",
    #         "zoomIn2d",
    #         "zoomInGeo",
    #         "zoomInMapbox",
    #         "zoomOut2d",
    #         "zoomOutGeo",
    #         "zoomOutMapbox",
    #         "zoomin",
    #         "zoomout",
    #         "displaylogo"
    #       )
    #     )
    #   )

  
  
  
  # Clear highlights when aggregation changes
  observeEvent(input$aggregation, {
    #selected_dataset_id(NULL)
    leafletProxy("map") |>
      clearGroup("highlight_points")
  })

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


  # Makes sure that this object exists even before the first clicking event
  selected_object <- reactiveVal("")

  grassland_renamed <- reactive({
    dataset_i() |>
      rename(column_y = input$column_y)
  })
  
  # grassland_inbounds <- reactive({
  #   if (length(ranges()) > 0) {
  #     ranges <- ranges()[[1]]
  #     lat <- ranges[, 2]
  #     lng <- ranges[, 1]
  #     geodata_i |>
  #       filter(
  #         lange > min(lng),
  #         lange < max(lng),
  #         breite > min(lat),
  #         breite < max(lat)
  #       )
  #   } else {
  #     geodata_i[FALSE, ]
  #   }
  # })



  grassland_inbounds_renamed <- reactive({
    grassland_inbounds <- grassland_inbounds() |>
      rename(column_y = input$column_y)
    return(grassland_inbounds)
  })
})
