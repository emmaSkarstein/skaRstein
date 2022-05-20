
#' Convert Python array of colors from Coolors.co to R-vector of strings
#'
#' @param array_string Python string of color hex codes
#' @param n Number of colors in string, default 5
#'
#' @return Vector of hex codes, usable in R
#' @export
#'
#' @examples
CreateRPalette <- function(array_string, n = 5){
  # Take in a Python (?) array (as a string) from Coolors to use in R.

  # Remove brackets in the beginning and end
  pal1 <- stringr::str_sub(array_string, start = 2, end = -2)

  # Make into a character vector
  pal2 <- stringr::str_split_fixed(pal1, ",", n = n)
  pal2a <- stringr::str_sub(pal2, start = 2, end = -2)

  # Add # to each color
  pal3 <- paste("#", pal2a, sep = "")

  return(pal3)
}

#' Show colors
#'
#' @param palette Vector of hex-codes to be visualized
#'
#' @return ggplot2 object that shows the colors
#' @export
#'
#' @examples
ShowColors <- function(palette){
  p <- ggplot2::ggplot() +
    geom_bar(aes(x = 1, fill = as.factor(1:length(palette)))) +
    scale_fill_manual(values = palette) +
    coord_flip() +
    theme_void() +
    theme(legend.position = "none")
  return(p)
}
