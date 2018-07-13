# -----Header-----
# Title: balloon.r
# Author: Brian Balzar
# Description: Provide a balloon plot
# ----------------

#' balloon.r
#'
#' @param df Data frame. Must contain two categorical variables and two value variables.
#' @param cat_x First categorical variable. To be placed on "x-axis."
#' @param cat_y Second categorical variable. To be placed on "y-axis."
#' @param size Value variable to determine size of balloon.
#' @param fill Value variable to determine color of balloon.
#' @param palette An RColorBrewer palette. Defaults to "RdBu."
#'
#' @return A balloon plot
#' @export
#'
#' @examples
#' m <- balloon(mtcars, gear, vs, wt, mpg, palette = "PuOr")
balloon <- function(df, cat_x, cat_y, size, fill, palette = "RdBu") {

  # Quosure
  cat_x_quo <- dplyr::enquo(cat_x)
  cat_y_quo <- dplyr::enquo(cat_y)
  size_quo <- dplyr::enquo(size)
  fill_quo <- dplyr::enquo(fill)
  size_name <- deparse(substitute(size))
  fill_name <- deparse(substitute(fill))

  # Manipulate Data
  play <- df %>%
    group_by(!!cat_x_quo, !!cat_y_quo) %>%
    #summarize(size = 1)
    summarize(size_subs = mean(!!size_quo, na.rm = TRUE),
              fill_subs = mean(!!fill_quo, na.rm = TRUE))

  # Plot Data
  plot <- ggplot(play, aes_(x = cat_x_quo, y = cat_y_quo)) +
    geom_point(aes(size = size_subs, fill = fill_subs), shape = 21, color = "black") +
    coord_flip() +
    theme_minimal() +
    ggthemes::theme_few() +
    scale_fill_distiller(palette = palette, name = fill_name) +
    scale_size_continuous(name = size_name)

  return(plot)
}
