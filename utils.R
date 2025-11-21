#########################################################################################
# additional functions used 

#' change the column names: all characters uppercase, replace _ with " " and undo the encoding of the Umlaute
#' @param str string of the column names
#' @return a decoded string suited to display in the app
clean_names <- function(str) {
  str |>
    str_replace_all("_", " ") |>
    str_replace_all("a0e", "ä") |>
    str_replace_all("o0e", "ö") |>
    str_replace_all("u0e", "ü") |>
    str_to_title()
}



# from a list of datasets, select a perticular dataset based on the aggregation level and the "topic"
# select_dataset <-
#   function(list_of_datasets,
#            selected_aggregation,
#            selected_dataset,
#            sep = "_") {
#     layer_name <-
#       paste(selected_aggregation, selected_dataset, sep = sep)
#     na.omit(list_of_datasets[[layer_name]])
#   }


#' create a legend with different transparencies and colors according to thresholds, for the resurvey and historic polygons
#' @param bivariate_matrix a matrix with the color names (3x3 here)
#' @param include_css css file path
#' @return a customised legend
create_legend <- function(bivariate_matrix, attribute_y = "Attribute Y", include_css = "appdata/www/mycss.css") {
  
  stopifnot(nrow(bivariate_matrix) == ncol(bivariate_matrix))
  
  n_classes <- nrow(bivariate_matrix)
  
  bivariate_matrix_df <- tibble(
    colour = as.vector(bivariate_matrix), # colour names
    row = rep(rev(seq_len(n_classes)), times = n_classes), # "row": a column, row placement information for the colour - different colours
    col = rep(seq_len(n_classes), each = n_classes) # "col: a column, column placement information for the colour - different transparencies
  ) |>
    arrange(row) # arrange it to have transparencies etc. ordered
  
  #define text and labels
  row_col_style <- bivariate_matrix_df |>
    pmap_chr(\(colour, row, col){
      paste0(".row-", row, ".col-", col, "{", "background-color: ", colour, ";", "}")
    }) |>
    paste(collapse = " ") |>
    tags$style()
  
  # define y-axis labels
  y_axis_div <- tags$div(class = "ylabel", paste(clean_names(attribute_y),"→")) #<div class="ylabel">Anzahl Arten →</div>
  
  # define colour matrix
  matrix_div <- bivariate_matrix_df |>
    pmap(\(colour, row, col){
      tags$div(tags$div(paste(row, col, sep = "-"), class = "tooltip"), class = c("val", paste0("row-", row), paste0("col-", col)))
    }) |>
    tags$div(class = "matrix", style = "grid-template-columns: repeat(3, 50px); grid-auto-rows: 50px")
  
  #define x-axis labels
  empty_div <- tags$div(class = "xlabel") #<div class="xlabel"></div>
  x_axis_div <- tags$div(class = "xlabel", "# Beobachtungen→") #<div class="xlabel"># Beobachtungen→</div>
  
  # combine all the information in one legend
  tags$html(
    includeCSS(include_css),
    row_col_style,
    tags$div(y_axis_div, matrix_div, empty_div, x_axis_div, class = "container2"),
  )
}


#' create a legend with the 5 threshold colours, for the historic and resurvey data with no aggregations
#' @param bivariate_matrix a matrix with the color names (5x1 here)
#' @param include_css css file path
#' @return a customised legend
create_legend_punkte <- function(bivariate_matrix, attribute_y = "Attribute Y", include_css = "appdata/www/mycss.css") {

  n_classes <- nrow(bivariate_matrix)
  
  bivariate_matrix_df <- tibble( 
    colour = as.vector(bivariate_matrix), # colour names
    row = seq_len(n_classes), # "row": a column, row placement information for the colour - different colours
    col = rep(1, n_classes)  # "col: a column, column placement information for the colour - no transparencies here so just 1 for all
  ) |>
    arrange(desc(row))
  
  #define text and labels
  row_col_style <- bivariate_matrix_df |>
    pmap_chr(\(colour, row, col){
      paste0(".row-", row, ".col-", col, "{", "background-color: ", colour, ";", "}")
    }) |>
    paste(collapse = " ") |>
    tags$style()
  
  # define y-axis labels
  y_axis_div <- tags$div(class = "ylabel", paste(clean_names(attribute_y),"→"))
  
  # define colour matrix
  matrix_div <- bivariate_matrix_df |>
    pmap(\(colour, row, col){
      tags$div(tags$div(paste(row, col, sep = "-"), class = "tooltip"), class = c("val", paste0("row-", row), paste0("col-", col)))
    }) |>
    tags$div(class = "matrix", style = "grid-template-columns: repeat(1, 35px); grid-auto-rows: 35px")
  
  #define x-axis labels - we do not have any label for the x-axis
  empty_div <- tags$div(class = "xlabel")
  
  # combine all the information in one legend
  tags$html(
    includeCSS(include_css),
    row_col_style,
    tags$div(y_axis_div, matrix_div, empty_div, class = "container2"),
  )
}


#' create a legend with different transparencies and colors according to thresholds, for the delta polygons
#' @param bivariate_matrix a matrix with the color names (5x3 here)
#' @param include_css css file path
#' @return a customised legend
create_legend_delta_polygone <- function(bivariate_matrix, attribute_y = "Attribute Y", include_css = "appdata/www/mycss.css") {
 
  stopifnot(nrow(bivariate_matrix) == ncol(bivariate_matrix))
  n_classes <- nrow(bivariate_matrix)
  
  bivariate_matrix_df <- tibble(
    colour = as.vector(bivariate_matrix), # colour names
    row = rep(rev(seq_len(n_classes)), times = n_classes), # "row": a column, row placement information for the colour - different colours
    col = rep(seq_len(n_classes), each = n_classes) # "col: a column, column placement information for the colour - different transparencies
  ) |>
    arrange(row)
  
  #define text and labels
  row_col_style <- bivariate_matrix_df |>
    pmap_chr(\(colour, row, col){
      paste0(".row-", row, ".col-", col, "{", "background-color: ", colour, ";", "}")
    }) |>
    paste(collapse = " ") |>
    tags$style()

  # define y-axis labels
  y_axis_div <- tags$div(class = "ylabel", tags$br(),paste(clean_names(attribute_y))) # variable to display
  y_axis_div_2 <- tags$div(class = "ylabel_2", HTML("  &nbsp  &nbsp - &nbsp; ← &nbsp; 0 &nbsp; → &nbsp; +")) # scale explanation
  
  # define colour matrix
  matrix_div <- bivariate_matrix_df |>
    pmap(\(colour, row, col){
      tags$div(tags$div(paste(row, col, sep = "-"), class = "tooltip"), class = c("val", paste0("row-", row), paste0("col-", col)))
    }) |>
    tags$div(class = "matrix", style = "grid-template-columns: repeat(3, 50px); grid-auto-rows: 50px") 
  
  #define x-axis labels
  empty_div <- tags$div(class = "xlabel")
  x_axis_div <- tags$div(class = "xlabel", "# Beobachtungen→")
  
  # combine all the information in one legend
  tags$html(
    includeCSS(include_css),
    row_col_style,
    tags$div(y_axis_div,y_axis_div_2, matrix_div, empty_div, x_axis_div, class = "container2"),
  )
}


# from here:
# https://github.com/rstudio/gt/blob/ff878e10d21a3ba897c5f99801b796da8fb637fa/R/helpers.R#L2496-L2536
adjust_luminance <- function(colors, steps) {
  stopifnot(steps < 2, steps > -2)
  rgb_matrix <- t(grDevices::col2rgb(colors, alpha = TRUE)) / 255
  alpha <- rgb_matrix[, "alpha"]
  luv_matrix <-
    grDevices::convertColor(rgb_matrix[, 1:3], "sRGB", "Luv")
  h <- atan2(luv_matrix[, "v"], luv_matrix[, "u"]) * 180 / pi
  c <- sqrt(luv_matrix[, "u"]^2 + luv_matrix[, "v"]^2)
  l <- luv_matrix[, "L"]
  y <- l / 100.
  x <- log(-(y / (y - 1)))
  y_2 <- 1 / (1 + exp(-(x + steps)))
  l <- y_2 * 100.
  grDevices::hcl(h, c, l, alpha = alpha)
}

# create a matrix with color palette
bivariate_matrix_luminocity <-
  function(mypal,
           n = length(mypal),
           combine_with = "cbind") {
    accumulate(seq_len(n - 1), \(x, y) adjust_luminance(x, 1), .init = mypal) |>
      rev() |>
      (\(x) do.call(combine_with, x))()
  }

bivariate_matrix_alpha <-
  function(mypal,
           n = length(mypal),
           alpha_range = c(0, 1)) {
    rgb_mat <- col2rgb(mypal) / 255
    a_from <- alpha_range[1]
    a_to <- alpha_range[2]
    alpha_seq <- seq(a_from, a_to, (a_to - a_from) / (n - 1))
    
    sapply(alpha_seq, function(alpha) {
      apply(rgb_mat, 2, \(x) rgb(x[1], x[2], x[3], alpha))
    })
  }
