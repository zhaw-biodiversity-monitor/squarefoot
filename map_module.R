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

#' Update map with point data
#' @param map_proxy The leaflet proxy object
#' @param data The data to display
#' @param ycol The column to use for coloring
#' @param column_y The name of the column for the legend
update_map_points <- function(map_proxy, data, ycol, column_y) {
  
  # browser()
  threshold_w <- threshold[threshold$Parameter == column_y,] |> 
    pivot_longer(-Parameter)
  
  
  ycol_labs <- cut(ycol, c(-Inf,threshold_w$value,Inf),labels = c(threshold_w$name[1:2],"Gleichbleibend",threshold_w$name[3:4]))

  pal_col <- RColorBrewer::brewer.pal(length(levels(ycol_labs)), "RdYlBu")
  
  
  # Use inverted color scale if not "Feuchtezahl" or "Reaktionszahl"
  if (!(column_y %in% c("feuchtezahl", "reaktionszahl"))) {
    pal_col <- rev(pal_col)
  } 

  pal <- colorFactor(palette = pal_col, domain = ycol_labs)
  
  
  # Create popup content
  popup_content <- paste0(
    #"<strong>Dataset ID:</strong> ", data$dataset_id, "<br>",
    "<strong>Time:</strong> ", data$time, "<br>"
  )
  
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
      # weight = 100,
      stroke = FALSE,
      fillOpacity = 1, 
      opacity = 1,
      popup = popup_content,
      #layerId = ~dataset_id,
      group = "main_points"
    ) |>
    # Add highlight layer (initially invisible)
    addCircleMarkers(
      data = data,
      # fillColor = ~pal(ycol_labs),
      # radius = 12,
      color = "white",
      fillOpacity = 0,
      opacity = 0,
      group = "highlight_points"
    ) |>
    addLegend(
      "bottomright",
      pal = pal,
      values = ycol_labs,
      title = clean_names(column_y),
      opacity = 1
    )
}

#' Update map with polygon data
#' @param map_proxy The leaflet proxy object
#' @param data The data to display
#' @param ycol The column to use for coloring
#' @param n_obs The number of observations
#' @param column_y The name of the column for the legend
update_map_polygons <- function(map_proxy, data, ycol, n_obs, column_y, n_classes = 3) {
  
  # browser()
  fac_levels <- expand_grid(seq_len(n_classes), seq_len(n_classes)) |>
    apply(1, paste, collapse = "-")
  
  n_obs_interval <- classIntervals(n_obs, n_classes, "jenks")
  ycol_interval <- classIntervals(ycol, n_classes, "jenks")
  
  n_obs_grp <- findCols(n_obs_interval)
  ycol_grp <- findCols(ycol_interval)
  
  data$grp <- factor(paste(n_obs_grp, ycol_grp, sep = "-"), levels = fac_levels)
  
  bivariate_palette <- COLOR_CONFIG$bivariate_palette
  
  if (!(column_y %in% c("feuchtezahl", "reaktionszahl"))) {
    bivariate_palette <- rev(bivariate_palette)
  } 
  
  
  bivariate_matrix <- bivariate_matrix_alpha(
    bivariate_palette,
    n_classes,
    alpha_range = c(.40, 0.95)
  )
  
  legend_html <- create_legend(bivariate_matrix, column_y)
  pal_col <- as.vector(bivariate_matrix)
  pal <- colorFactor(pal_col, levels = fac_levels, alpha = TRUE)
  
  # browser()
  data$label <- paste(
    paste(str_to_title(column_y), format(round(ycol, 3)), sep = ":"),
    paste("Anzahl Erhebungen", n_obs, sep = ":"),
    sep = "<br>"
  )
  
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
