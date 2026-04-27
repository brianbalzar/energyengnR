# ----------
# Author: Brian Balzar
# Title: mandelbrot.r
# Description: Generates the Mandelbrot Set
# ----------

#' mandelbrot
#'
#' Computes the Mandelbrot Set over a grid of complex numbers and returns a
#' data frame suitable for plotting with ggplot2.
#'
#' @param x_range Numeric vector of length 2. Real axis limits. Default c(-2.5, 1).
#' @param y_range Numeric vector of length 2. Imaginary axis limits. Default c(-1.25, 1.25).
#' @param resolution Integer. Number of points along each axis. Default 500.
#' @param max_iter Integer. Maximum iterations before a point is considered inside the set. Default 100.
#'
#' @return A data frame with columns x, y, and iter. Points inside the set
#'   have iter equal to max_iter.
#' @export
#'
#' @examples
#' m <- mandelbrot()
#' ggplot2::ggplot(m, ggplot2::aes(x, y, fill = iter)) +
#'   ggplot2::geom_raster() +
#'   ggplot2::scale_fill_viridis_c() +
#'   ggplot2::coord_equal() +
#'   ggplot2::theme_void()
#'
#' # Zoom into the seahorse valley
#' m <- mandelbrot(x_range = c(-0.8, -0.7), y_range = c(0.05, 0.15), resolution = 800, max_iter = 300)
mandelbrot <- function(x_range = c(-2.5, 1),
                       y_range = c(-1.25, 1.25),
                       resolution = 500,
                       max_iter = 100) {

  xs <- seq(x_range[1], x_range[2], length.out = resolution)
  ys <- seq(y_range[1], y_range[2], length.out = resolution)

  grid <- expand.grid(x = xs, y = ys)
  c_vals <- complex(real = grid$x, imaginary = grid$y)

  iter_counts <- mandelbrot_iter(c_vals, max_iter)

  grid$iter <- iter_counts
  grid
}

mandelbrot_iter <- function(c_vals, max_iter) {
  n <- length(c_vals)
  z <- complex(n)
  iter <- integer(n)
  active <- rep(TRUE, n)

  for (i in seq_len(max_iter)) {
    z[active] <- z[active]^2 + c_vals[active]
    escaped <- active & Mod(z) > 2
    iter[escaped] <- i
    active[escaped] <- FALSE
    if (!any(active)) break
  }

  iter[active] <- max_iter
  iter
}
