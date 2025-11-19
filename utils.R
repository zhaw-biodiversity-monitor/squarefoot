

read_all_layers <- function(file, exception = NA) {
  layer_names <- st_layers(file)$name
  layer_names <- layer_names[!(layer_names %in% exception)]
  sapply(layer_names, \(x)st_read(file, x), simplify = FALSE)
}

clean_names <- function(str) {
  str |>
    str_replace_all("_", " ") |>
    str_replace_all("a0e", "ä") |>
    str_replace_all("o0e", "ö") |>
    str_replace_all("u0e", "ü") |>
    str_to_title()
}


# add_unit <- function(input, add_break = TRUE, add_brackets = TRUE) {
#   unit <- case_when(
#     str_detect(input, "artenreichtum") ~ "Anzahl Arten",
#     str_detect(input, "anteil") ~ "Anteil Neophyten zwischen 0-1",
#     str_detect(input, "temperaturzahl") ~ "Temperaturzahl",
#     str_detect(input, "kontinentalitatszahl") ~ "Kontinentalitätszahl",
#     str_detect(input, "freuchtezahl") ~ "Feuchtezahl",
#     str_detect(input, "reaktionszahl") ~ "Reaktionszahl",
#     str_detect(input, "nahrstoffzahl") ~ "Nährstoffzahl",
#     str_detect(input, "strategie_c") ~ "Konkurrenzzahl",
#     str_detect(input, "strategie_s") ~ "Stresszahl",
#     str_detect(input, "strategie_r") ~ "Ruderalzahl",
#     .default = ""
#   )
# 
#   if (add_brackets & unit != "") {
#     unit <- paste0("(", unit, ")")
#   }
# 
#   if (add_break & unit != "") {
#     unit <- paste0("\n", unit)
#   }
# 
#   unit
# }

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



create_legend <- function(bivariate_matrix, attribute_y = "Attribute Y", include_css = "appdata/www/mycss.css") {
  stopifnot(nrow(bivariate_matrix) == ncol(bivariate_matrix))
  n_classes <- nrow(bivariate_matrix)

  bivariate_matrix_df <- tibble(
    colour = as.vector(bivariate_matrix),
    row = rep(rev(seq_len(n_classes)), times = n_classes), # "row": a column, needs to be 1 to 5 for all the five colours
    col = rep(seq_len(n_classes), each = n_classes) # "col: a column, needs to be 1 for all colours
  ) |>
    arrange(row)
  row_col_style <- bivariate_matrix_df |> #rearrange the palette but maybe not needed as we do not have a 3x3 grid
    pmap_chr(\(colour, row, col){
      paste0(".row-", row, ".col-", col, "{", "background-color: ", colour, ";", "}")
    }) |>
    paste(collapse = " ") |>
    tags$style()



  y_axis_div <- tags$div(class = "ylabel", paste(clean_names(attribute_y),"→")) #<div class="ylabel">Anzahl Arten →</div> # ahnzahl arten und mit 0 und pfeil nach oben, bei delta anzahl arten mit 0 in mitte und +/- pfeile nach oben und unten
  matrix_div <- bivariate_matrix_df |> #?????????
    pmap(\(colour, row, col){
      tags$div(tags$div(paste(row, col, sep = "-"), class = "tooltip"), class = c("val", paste0("row-", row), paste0("col-", col)))
    }) |>
    tags$div(class = "matrix", style = "grid-template-columns: repeat(3, 50px); grid-auto-rows: 50px") #hardcoded, remove

  empty_div <- tags$div(class = "xlabel") #<div class="xlabel"></div>
  x_axis_div <- tags$div(class = "xlabel", "# Beobachtungen→") #<div class="xlabel"># Beobachtungen→</div>  -- weglah

  tags$html(
    includeCSS(include_css),
    row_col_style,
    tags$div(y_axis_div, matrix_div, empty_div, x_axis_div, class = "container2"),
  )
}

create_legend_punkte <- function(bivariate_matrix, attribute_y = "Attribute Y", include_css = "appdata/www/mycss.css") {

  #stopifnot(nrow(bivariate_matrix) == ncol(bivariate_matrix))
  n_classes <- nrow(bivariate_matrix)
  
  bivariate_matrix_df <- tibble( 
    colour = as.vector(bivariate_matrix),
    row = seq_len(n_classes),
    col = rep(1, n_classes) 
  ) |>
    arrange(desc(row))
  row_col_style <- bivariate_matrix_df |>
    pmap_chr(\(colour, row, col){
      paste0(".row-", row, ".col-", col, "{", "background-color: ", colour, ";", "}")
    }) |>
    paste(collapse = " ") |>
    tags$style()
  
  y_axis_div <- tags$div(class = "ylabel", paste(clean_names(attribute_y),"→"))
  matrix_div <- bivariate_matrix_df |>
    pmap(\(colour, row, col){
      tags$div(tags$div(paste(row, col, sep = "-"), class = "tooltip"), class = c("val", paste0("row-", row), paste0("col-", col)))
    }) |>
    tags$div(class = "matrix", style = "grid-template-columns: repeat(1, 35px); grid-auto-rows: 35px") #hardcoded, remove
  
  empty_div <- tags$div(class = "xlabel")
  # x_axis_div <- tags$div(class = "xlabel", "# Beobachtungen→")
  
  tags$html(
    includeCSS(include_css),
    row_col_style,
    tags$div(y_axis_div, matrix_div, empty_div, class = "container2"),
  )
}

create_legend_delta_polygone <- function(bivariate_matrix, attribute_y = "Attribute Y", include_css = "appdata/www/mycss.css") {
 
  stopifnot(nrow(bivariate_matrix) == ncol(bivariate_matrix))
  n_classes <- nrow(bivariate_matrix)
  
  bivariate_matrix_df <- tibble(
    colour = as.vector(bivariate_matrix),
    row = rep(rev(seq_len(n_classes)), times = n_classes),
    col = rep(seq_len(n_classes), each = n_classes)
  ) |>
    arrange(row)
  row_col_style <- bivariate_matrix_df |>
    pmap_chr(\(colour, row, col){
      paste0(".row-", row, ".col-", col, "{", "background-color: ", colour, ";", "}")
    }) |>
    paste(collapse = " ") |>
    tags$style()

  y_axis_div <- tags$div(class = "ylabel", tags$br(),paste(clean_names(attribute_y)))#, style = "text-align: left;")
  y_axis_div_2 <- tags$div(class = "ylabel_2", HTML("  &nbsp  &nbsp - &nbsp; ← &nbsp; 0 &nbsp; → &nbsp; +"))#, style = "text-align: left;")
  
  # y_axis_div_combined <- tags$div(
  #   class = "ylabel-container",
  #   y_axis_div,
  #   y_axis_div_2
  # )
  
  matrix_div <- bivariate_matrix_df |>
    pmap(\(colour, row, col){
      tags$div(tags$div(paste(row, col, sep = "-"), class = "tooltip"), class = c("val", paste0("row-", row), paste0("col-", col)))
    }) |>
    tags$div(class = "matrix", style = "grid-template-columns: repeat(3, 50px); grid-auto-rows: 50px") #hardcoded, remove
  
  empty_div <- tags$div(class = "xlabel")
  x_axis_div <- tags$div(class = "xlabel", "# Beobachtungen→")
  
  tags$html(
    includeCSS(include_css),
    row_col_style,
    tags$div(y_axis_div,y_axis_div_2, matrix_div, empty_div, x_axis_div, class = "container2"),
  )
}