
# Basic theme ==================================================================

#' My basic theme
#'
#' @param base_size Base font size
#' @param base_family Base font family
#'
#' @return a basic ggplot theme with my preferred settings
#' @export
#'
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + my_basic_theme()
my_basic_theme <- function(base_size = 12,
                           base_family = 'sans'){
  return_theme <- ggplot2::theme(
    # TEXT FORMAT
    text = element_text(family = base_family, size = base_size),
    axis.text = element_text(size = 10,
                             color = "black"),
    plot.title = element_text(size = 20,
                              margin = margin(b = 5)),
    plot.subtitle = element_text(size = 15,
                                 margin = margin(b = 10)),

    # AXIS FORMAT
    axis.title = element_blank(),

    # LEGEND FORMAT
    legend.position = "top",
    legend.title = element_blank(),
    legend.justification = 0,
    legend.text = element_text(size = 15,
                               family = base_family),

    # BACKGROUND FORMAT
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#F9F8F8"),
    panel.border = element_rect(color = "gray", fill = NA))

  return(return_theme)
}
