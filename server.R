source("libraries.R")
source("config.R")
source("utils.R")
source("map_module.R")
source("data_module.R")

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
  
  
  filtered_data <- reactive({
    filter_data(
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
      
      # Filter data for points
      # filtered_data <- filter_data(
      #   geodata_i,
      #   input$dataset,
      #   input$lebensraumgruppen,
      #   input$flaeche
      # )
      
      # Get column values
      ycol <- get_column_values(filtered_data(), input$column_y)
      
      # Update map with points
      leafletProxy("map") |>
        update_map_points(filtered_data(), ycol, input$column_y)
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
      ycol <- get_column_values(filtered_data(), input$column_y)
      
      maxval <- max(abs(range(ycol)))
      
      output$scatterplot <- plot_ly(x = ycol) |> 
        add_histogram() |> 
        layout(
          xaxis = list(title = clean_names(input$column_y), range = list(maxval*-1, maxval)),
          yaxis = list(title = "Häufigkeit")
        ) |> 
        renderPlotly()
      
      }
      
    })

  
  # Handle point clicks
  observeEvent(input$map_marker_click, {
    if (input$aggregation == "punkte") {
      click <- input$map_marker_click
      #dataset_id <- click$id
      
      # Update selected dataset ID
      #selected_dataset_id(dataset_id)
      
      # Get current data
      # filtered_data <- filter_data(
      #   geodata_i(),
      #   input$dataset,
      #   input$lebensraumgruppen,
      #   input$flaeche
      # )
      
      # Filter points with same dataset ID
      #highlight_data <- filtered_data()[filtered_data()$  dataset_id == dataset_id, ]
      
      # browser()
      
      # Update highlight layer
      leafletProxy("map") |>
        clearGroup("highlight_points") |>
        addCircleMarkers(
          data = highlight_data,
          radius = 8,
          color = "black",  # Black border
          fillOpacity = 0,
          opacity = 1,
          weight = 2,  # Thicker border
          group = "highlight_points"
        )
    }
  })
  
  # Clear highlights when aggregation changes
  observeEvent(input$aggregation, {
    #selected_dataset_id(NULL)
    leafletProxy("map") |>
      clearGroup("highlight_points")
  })

  ranges <- reactive({
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


  # Makes sure that this object exists even before the first clicking event
  selected_object <- reactiveVal("")

  grassland_renamed <- reactive({
    dataset_i() |>
      rename(column_y = input$column_y)
  })



  grassland_inbounds_renamed <- reactive({
    grassland_inbounds <- grassland_inbounds() |>
      rename(column_y = input$column_y)
    return(grassland_inbounds)
  })
})
