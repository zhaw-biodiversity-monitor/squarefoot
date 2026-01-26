#########################################################################################
# Map Module Functions

#' Initialize the base map
#' @return A leaflet map object
init_map <- function() {
  leaflet() |>
    addTiles(
      MAP_CONFIG$tile_layers$grau,
      group = "Pixelkarte grau"
    ) |>
    addTiles(
      MAP_CONFIG$tile_layers$swissimage,
      group = "Swissimage"
    ) |>
    addTiles(
      MAP_CONFIG$tile_layers$farbig,
      group = "Pixelkarte farbig"
    ) |>
    addLayersControl(baseGroups = c("Pixelkarte grau", "Pixelkarte farbig", "Swissimage")) |>
    fitBounds(
      MAP_CONFIG$bounds$west,
      MAP_CONFIG$bounds$south,
      MAP_CONFIG$bounds$east,
      MAP_CONFIG$bounds$north
    )
}


#' Update map with point data if no aggregation is chosen
#' @param map_proxy The leaflet proxy object
#' @param data The data to display
#' @param ycol The column to use for coloring
#' @param column_y The name of the column for the legend
#' @param threshold data from the file threshold_definitions for the color coding
#' @param time_aspect information about what time subset is used to adjust thresholds
update_map_points <- function(map_proxy, data, ycol, column_y, threshold, time_aspect){
  
  if (time_aspect == "delta"){
    #make thresholds
    threshold_w <- threshold[threshold$Parameter == column_y,] |> 
      pivot_longer(-Parameter)
    ycol_labs <- cut(ycol, c(-Inf,threshold_w$value,Inf),labels = c(threshold_w$name[1:2],"Gleichbleibend",threshold_w$name[3:4]))

    # organise colour palette
    pal_col <- RColorBrewer::brewer.pal(length(levels(ycol_labs)), "RdYlBu")
    
    # Use inverted color scale for temperature
    if (column_y == "temperaturzahl") {
      pal_col <- rev(pal_col)
    }
    
    pal <- colorFactor(palette = pal_col, domain = ycol_labs)
    pal_legend <- colorFactor(palette = rev(pal_col), domain = ycol_labs) # reverse the colors and labels to have red at the bottom and blue at the top
    
    
    # add popup content when hovering over the points
    popup_content <- paste(
      paste(clean_names(column_y), format(round(ycol, 3)), sep = ": "),
      sep = "<br>"
    )
    
    # plot the coloured points on the map
    map_proxy |>
      clearShapes() |>
      clearMarkers() |> 
      clearControls() |>
      # Add main points layer
      addCircleMarkers(
        data = data,
        fillColor = ~pal(ycol_labs),
        radius = 8, 
        color = "black",
        stroke = FALSE,
        fillOpacity = 1, 
        opacity = 1,
        label = popup_content, 
        group = "main_points"
      ) |>
      addLegend(
        "bottomleft",
        pal = pal_legend,
        values = ycol_labs,
        title = clean_names(column_y),
        opacity = 1
      )
  }
  
  else{ #historic und resurvey
    #make thresholds
    threshold_w <- threshold[threshold$Parameter == column_y,] |> 
      pivot_longer(-Parameter)
    ycol_labs <- cut(ycol, c(-Inf,threshold_w$value,Inf),labels = c(threshold_w$name[1:2],"Mittel",threshold_w$name[3:4])) # add for the colours but name is not important as the legend is done in utils/create_legend_punkte
    
    # organise colour palette
    pal_col <- RColorBrewer::brewer.pal(length(levels(ycol_labs)), "RdYlBu")
    if (!(column_y %in% c("feuchtigkeitszahl", "reaktionszahl"))) { # Use inverted color scale if not "feuchtigkeit" or "reaktion" because the intuitive colours would not match (feuchtigkeit being red when high, reaction being red when alkaline)
      pal_col <- rev(pal_col)
    }
    pal <- colorFactor(palette = pal_col, domain = ycol_labs)
    pal_col_matrix <- matrix(pal_col, nrow=5, ncol=1)
    
    # create the custom legend from utils
    legend_html <- create_legend_punkte(pal_col_matrix, column_y)
    
    # create popup content to display when hovering over datapoints
    popup_content <- paste(
      paste(clean_names(column_y), format(round(ycol, 3)), sep = ": "),
      sep = "<br>"
    )
    
    # plot the coloured points on the map
    map_proxy |>
      clearShapes() |>
      clearMarkers() |> 
      clearControls() |>
      addControl(legend_html, position = "bottomleft", className = "")|>
      # Add main points layer
      addCircleMarkers(
        data = data,
        fillColor = ~pal(ycol_labs),
        radius = 8, 
        color = "black",
        stroke = FALSE,
        fillOpacity = 1, 
        opacity = 1,
        label = popup_content, 
        group = "main_points"
      )  
    }
}

#' Update map with polygon data if any aggregation is chosen
#' @param map_proxy The leaflet proxy object
#' @param data The data to display
#' @param ycol The column to use for coloring
#' @param n_obs number of observations in the polygons
#' @param column_y The name of the column for the legend
#' @param n_classes nr of colours in the legend
#' @param threshold data from the file threshold_definitions for the color coding
#' @param time_aspect information about what time subset is used to adjust thresholds
update_map_polygons <- function(map_proxy, data, ycol, n_obs, column_y, n_classes = 3,threshold, time_aspect) {
  
  if (time_aspect == "delta"){ 
    #make thresholds
    threshold_w <- threshold[threshold$Parameter == column_y, ] |>
       pivot_longer(-Parameter)
    threshold_w <- threshold_w[2:3,] #merge the two threshold at the extremes, to have only 3 color groups instead of 5
    
    fac_levels <- expand_grid(seq_len(n_classes), seq_len(n_classes)) |>
      apply(1, paste, collapse = "-")
    
    # make intervals 
    n_obs_interval <- classIntervals(n_obs, n_classes, "jenks") # function to make intervals, for the n_obs/transparency of colours
    ycol_labs <- cut(ycol, c(-Inf,threshold_w$value,Inf),labels = c(1,2,3)) # custom intervals from the thresholds, for the colours
    # group polygons into intervals
    n_obs_grp <- findCols(n_obs_interval)
    ycol_grp <- ycol_labs
    
    # match observations into factor levels
    data$grp <- factor(paste(n_obs_grp, ycol_grp, sep = "-"), levels = fac_levels)
    
    # make the color palette
    bivariate_palette <- RColorBrewer::brewer.pal(5, "RdYlBu")
    bivariate_palette <- bivariate_palette[c(1,3,5)] # take blue yellow and red instead of the standard light blue orange and yellow when inputting 3 classes in colorbrewer

    # reverse temperature
    if (column_y == "temperaturzahl") {
      bivariate_palette <- rev(bivariate_palette)
     }
    
    bivariate_matrix <- bivariate_matrix_alpha(
      bivariate_palette,
      n_classes,
      alpha_range = c(.40, 0.95)
    )
    pal_col <- as.vector(bivariate_matrix)


    pal <- colorFactor(pal_col, levels = fac_levels, alpha = TRUE)
    
    # create a custom legend (utils)
    legend_html <- create_legend_delta_polygone(bivariate_matrix, column_y) 
    
    # make popup label when hovering over the polygons
    data$label <- paste(
      paste(clean_names(column_y), format(round(ycol, 3)), sep = ": "),
      paste("Anzahl Erhebungen", n_obs, sep = ": "),
      sep = "<br>"
    )
    
    # paint polygons on the map
    map_proxy |>
      clearShapes() |>
      clearControls() |>
      addControl(legend_html, position = "bottomleft", className = "") |>
      addPolygons(
        data = data,
        fillColor = ~pal(grp),
        color = ~pal(grp),
        fillOpacity = 1,
        opacity = 0,
        label = ~lapply(label, htmltools::HTML)
      )
  }
  
  else{ # resurvey or historic
    #make thresholds
    threshold_w <- threshold[threshold$Parameter == column_y, ] |>
      pivot_longer(-Parameter)
    threshold_w <- threshold_w[2:3,] #merge the two threshold at the extremes colors to have only 3 color groups
    
    fac_levels <- expand_grid(seq_len(n_classes), seq_len(n_classes)) |>
      apply(1, paste, collapse = "-")
    
    # make intervals
    n_obs_interval <- classIntervals(n_obs, n_classes, "jenks") # function to make intervals, for the n_obs/transparency of colours
    ycol_labs <- cut(ycol, c(-Inf,threshold_w$value,Inf),labels = c(1,2,3))# custom intervals from the thresholds, for the colours
    # group polygons into intervals
    n_obs_grp <- findCols(n_obs_interval)
    ycol_grp <- ycol_labs
    
    # match observations into factor levels
    data$grp <- factor(paste(n_obs_grp, ycol_grp, sep = "-"), levels = fac_levels)
    
    # make the color palette
    bivariate_palette <- RColorBrewer::brewer.pal(5, "RdYlBu")
    bivariate_palette <- bivariate_palette[c(1,3,5)] # take blue yellow and red instead of the standard light blue orange and yellow when inputting 3 classes in colorbrewer
    # Use inverted color scale if not "feuchtigkeit" or "reaktion" because the intuitive colours would not match (feuchtigkeit being red when high, reaction being red when alkaline)
    if (!(column_y %in% c("feuchtigkeitszahl", "reaktionszahl"))) {
      bivariate_palette <- rev(bivariate_palette)
    }
    bivariate_matrix <- bivariate_matrix_alpha(
      bivariate_palette,
      n_classes,
      alpha_range = c(.40, 0.95)
    )
    pal_col <- as.vector(bivariate_matrix)
    pal <- colorFactor(pal_col, levels = fac_levels, alpha = TRUE)

    # create a custom legend (utils)
    legend_html <- create_legend(bivariate_matrix, column_y)

    # make popup label when hovering over the polygons
    data$label <- paste(
      paste(clean_names(column_y), format(round(ycol, 3)), sep = ":"),
      paste("Anzahl Erhebungen", n_obs, sep = ": "),
      sep = "<br>"
    )
    
    # paint polygons on the map
    map_proxy |>
      clearShapes() |>
      clearControls() |>
      addControl(legend_html, position = "bottomleft", className = "") |>
      addPolygons(
        data = data,
        fillColor = ~pal(grp),
        color = ~pal(grp),
        fillOpacity = 1,
        opacity = 0,
        label = ~lapply(label, htmltools::HTML)
      )
  }
} 


#' add manual color scale for the scatterplot
#' @param threshold data from the file threshold_definitions for the color coding
#' @param input_t information about what time subset is used to adjust thresholds
#' @param col_y variable to be displayed
#' @param data data for the points
#' @param type_col type for the colouring "light" or "dark" - aggregation or not
#' @return data with colors added as column
add_color_scale <- function(threshold, input_t, col_y, data, type_col){
  
  if (input_t == "delta") { # get color palette for delta
    #make thresholds
    threshold_w <- threshold[threshold$Parameter == col_y,] |> 
      pivot_longer(-Parameter)
    ycol_labs <- cut(data$column_y, c(-Inf,threshold_w$value,Inf),labels = c(threshold_w$name[1:2],"Gleichbleibend",threshold_w$name[3:4])) # add for the colours but name is not used later so not important
    data$y_col_labs <- ycol_labs # add it to the data
      
    # make colour palette
    pal_col <- RColorBrewer::brewer.pal(length(levels(ycol_labs)), "RdYlBu")
    pal_col[3] <- colorRampPalette(c(pal_col[3], "yellow"))(3)[2] #sandybrown, lightsalmon, gold - custom yellow as it is not very well visible on the white background
    
    # Use inverted color scale for temperature
    if (col_y == "temperaturzahl") {
      pal_col <- rev(pal_col)
    }
    
    # lighten the colour if input is "light"- to make aggregations and non-aggregated data visibly differ enough  
    if (type_col=="light"){
      pal_col <- lighten(pal_col, amount = 0.2)
    } 
    else{
      pal_col <- darken(pal_col,amount = 0.6)
    }
      pal <- colorFactor(palette = pal_col, domain = ycol_labs)
  } 
  
  else{ # historic and resurvey
    #make thresholds
    threshold_w <- threshold_res_hist[threshold_res_hist$Parameter == col_y,] |> 
      pivot_longer(-Parameter)
    ycol_labs <- cut(data$column_y, c(-Inf,threshold_w$value,Inf),labels = c(threshold_w$name[1:2],"Mitte",threshold_w$name[3:4]))
    data$y_col_labs <- ycol_labs
    
    # make colour palette
    pal_col <- RColorBrewer::brewer.pal(length(levels(ycol_labs)), "RdYlBu")
    pal_col[3] <- colorRampPalette(c(pal_col[3], "yellow"))(3)[2] #sandybrown, lightsalmon, goldv - custom yellow as it is not very well visible on the white background
    #pal_col[pal_col == "#FFFFBF"] <- darken("#FFFFBF", amount = 0.03) #darken just the yellow so it is visible on the white background
     
    # Use inverted color scale if not "Feuchtezahl" or "Reaktionszahl" because the intuitive colours would not match (feuchtigkeit being red when high, reaction being red when alkaline)
    if (!(col_y %in% c("feuchtigkeitszahl", "reaktionszahl"))) {
      pal_col <- rev(pal_col)
    }
    
    # lighten the colour if input is "light"- to make aggregations and non-aggregated data visibly differ enough  
    if (type_col=="light"){
      pal_col <- lighten(pal_col, amount = 0.2)
    } 
    else{
      pal_col <- darken(pal_col,amount = 0.6)
    }
    pal <- colorFactor(palette = pal_col, domain = ycol_labs)
  }
  
  # add the colour palette to the data
  data$plot_color <- pal(data$y_col_labs)
  
  return(data)
}