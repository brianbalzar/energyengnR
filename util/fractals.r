# ----------
# Author: Brian Balzar
# Title: fractals.r
# Description: Companion fractal generators and utilities for mandelbrot.r
# ----------


#' julia
#'
#' Computes a Julia Set for a fixed complex parameter c over a grid of starting
#' points. Each (x, y) pair is the initial z value; the algorithm iterates
#' z = z^2 + c and records how many steps before escape.
#'
#' @param c_real Numeric. Real part of the fixed complex constant c. Default -0.7.
#' @param c_imag Numeric. Imaginary part of the fixed complex constant c. Default 0.27.
#' @param x_range Numeric vector of length 2. Real axis limits. Default c(-1.5, 1.5).
#' @param y_range Numeric vector of length 2. Imaginary axis limits. Default c(-1.5, 1.5).
#' @param resolution Integer. Number of points along each axis. Default 500.
#' @param max_iter Integer. Maximum iterations before a point is considered inside the set. Default 100.
#'
#' @return A data frame with columns x, y, and iter.
#' @export
#'
#' @examples
#' j <- julia(c_real = -0.4, c_imag = 0.6)
#' plot_fractal(j)
#'
#' # Dendrite Julia set
#' j <- julia(c_real = 0, c_imag = 1, max_iter = 200)
julia <- function(c_real = -0.7, c_imag = 0.27,
                  x_range = c(-1.5, 1.5),
                  y_range = c(-1.5, 1.5),
                  resolution = 500,
                  max_iter = 100) {

  xs <- seq(x_range[1], x_range[2], length.out = resolution)
  ys <- seq(y_range[1], y_range[2], length.out = resolution)

  grid <- expand.grid(x = xs, y = ys)
  z_vals <- complex(real = grid$x, imaginary = grid$y)
  c_val  <- complex(real = c_real, imaginary = c_imag)

  grid$iter <- julia_iter(z_vals, c_val, max_iter)
  grid
}

julia_iter <- function(z_vals, c_val, max_iter) {
  n      <- length(z_vals)
  z      <- z_vals
  iter   <- integer(n)
  active <- rep(TRUE, n)

  for (i in seq_len(max_iter)) {
    z[active] <- z[active]^2 + c_val
    escaped   <- active & Mod(z) > 2
    iter[escaped]  <- i
    active[escaped] <- FALSE
    if (!any(active)) break
  }

  iter[active] <- max_iter
  iter
}


#' burning_ship
#'
#' Computes the Burning Ship fractal. At each step the real and imaginary parts
#' of z are replaced with their absolute values before squaring:
#' z = (|Re(z)| + i|Im(z)|)^2 + c.
#'
#' @param x_range Numeric vector of length 2. Real axis limits. Default c(-2.5, 1.5).
#' @param y_range Numeric vector of length 2. Imaginary axis limits. Default c(-2, 0.5).
#' @param resolution Integer. Number of points along each axis. Default 500.
#' @param max_iter Integer. Maximum iterations before a point is considered inside the set. Default 100.
#'
#' @return A data frame with columns x, y, and iter.
#' @export
#'
#' @examples
#' b <- burning_ship()
#' plot_fractal(b, palette = "plasma")
burning_ship <- function(x_range = c(-2.5, 1.5),
                         y_range = c(-2, 0.5),
                         resolution = 500,
                         max_iter = 100) {

  xs <- seq(x_range[1], x_range[2], length.out = resolution)
  ys <- seq(y_range[1], y_range[2], length.out = resolution)

  grid   <- expand.grid(x = xs, y = ys)
  c_vals <- complex(real = grid$x, imaginary = grid$y)

  grid$iter <- burning_ship_iter(c_vals, max_iter)
  grid
}

burning_ship_iter <- function(c_vals, max_iter) {
  n      <- length(c_vals)
  z      <- complex(n)
  iter   <- integer(n)
  active <- rep(TRUE, n)

  for (i in seq_len(max_iter)) {
    z_abs     <- complex(real = abs(Re(z[active])), imaginary = abs(Im(z[active])))
    z[active] <- z_abs^2 + c_vals[active]
    escaped   <- active & Mod(z) > 2
    iter[escaped]   <- i
    active[escaped] <- FALSE
    if (!any(active)) break
  }

  iter[active] <- max_iter
  iter
}


#' tricorn
#'
#' Computes the Tricorn (Mandelbar) Set. The iteration rule conjugates z before
#' squaring: z = Conj(z)^2 + c. This breaks the rotational symmetry of the
#' Mandelbrot Set and produces a three-lobed shape.
#'
#' @param x_range Numeric vector of length 2. Real axis limits. Default c(-2.5, 1).
#' @param y_range Numeric vector of length 2. Imaginary axis limits. Default c(-1.25, 1.25).
#' @param resolution Integer. Number of points along each axis. Default 500.
#' @param max_iter Integer. Maximum iterations before a point is considered inside the set. Default 100.
#'
#' @return A data frame with columns x, y, and iter.
#' @export
#'
#' @examples
#' t <- tricorn()
#' plot_fractal(t, palette = "magma")
tricorn <- function(x_range = c(-2.5, 1),
                    y_range = c(-1.25, 1.25),
                    resolution = 500,
                    max_iter = 100) {

  xs <- seq(x_range[1], x_range[2], length.out = resolution)
  ys <- seq(y_range[1], y_range[2], length.out = resolution)

  grid   <- expand.grid(x = xs, y = ys)
  c_vals <- complex(real = grid$x, imaginary = grid$y)

  grid$iter <- tricorn_iter(c_vals, max_iter)
  grid
}

tricorn_iter <- function(c_vals, max_iter) {
  n      <- length(c_vals)
  z      <- complex(n)
  iter   <- integer(n)
  active <- rep(TRUE, n)

  for (i in seq_len(max_iter)) {
    z[active] <- Conj(z[active])^2 + c_vals[active]
    escaped   <- active & Mod(z) > 2
    iter[escaped]   <- i
    active[escaped] <- FALSE
    if (!any(active)) break
  }

  iter[active] <- max_iter
  iter
}


#' plot_fractal
#'
#' Renders a fractal data frame (from mandelbrot, julia, burning_ship, or
#' tricorn) as a ggplot2 raster using a viridis color scale. Points inside
#' the set are drawn in the background color.
#'
#' @param df Data frame with columns x, y, and either iter or smooth_iter.
#'   Use smooth_color() first for banding-free output.
#' @param palette Character. A viridis palette name passed to
#'   \code{scale_fill_viridis_c}: "viridis", "magma", "plasma", "inferno",
#'   or "cividis". Default "inferno".
#' @param background Character. Fill color for points inside the set (iter ==
#'   max(iter)). Default "black".
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' plot_fractal(mandelbrot())
#' plot_fractal(julia(), palette = "plasma")
#' plot_fractal(smooth_color(mandelbrot(max_iter = 200)), palette = "magma")
plot_fractal <- function(df, palette = "inferno", background = "black") {

  col_name <- if ("smooth_iter" %in% names(df)) "smooth_iter" else "iter"
  fill_quo <- ggplot2::sym(col_name)

  ggplot2::ggplot(df, ggplot2::aes(x, y, fill = !!fill_quo)) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::scale_fill_viridis_c(option = palette, name = col_name) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = background, color = NA),
      plot.background  = ggplot2::element_rect(fill = background, color = NA),
      legend.text      = ggplot2::element_text(color = "white"),
      legend.title     = ggplot2::element_text(color = "white")
    )
}


#' smooth_color
#'
#' Adds a smooth_iter column to a fractal data frame by applying histogram
#' equalization to the escape iteration counts. This redistributes the color
#' mapping to match the actual density of escaping points, eliminating the
#' stepped banding that appears when using raw integer iter values.
#'
#' Points inside the set (iter == max_iter) receive smooth_iter = 0 so they
#' map to the low end of any diverging or sequential palette and stay distinct.
#'
#' @param df Data frame with columns x, y, and iter, as returned by
#'   mandelbrot, julia, burning_ship, or tricorn.
#' @param max_iter Integer. The iteration value that marks membership in the
#'   set. Inferred from max(df$iter) when NULL. Default NULL.
#'
#' @return The input data frame with an added smooth_iter column in [0, 1].
#' @export
#'
#' @examples
#' m <- mandelbrot(max_iter = 200) |> smooth_color()
#' plot_fractal(m, palette = "magma")
smooth_color <- function(df, max_iter = NULL) {

  if (is.null(max_iter)) max_iter <- max(df$iter)

  outside      <- df$iter < max_iter
  iter_outside <- df$iter[outside]
  total        <- sum(outside)

  counts      <- tabulate(iter_outside, nbins = max_iter - 1L)
  cdf         <- cumsum(counts) / total

  smooth             <- numeric(nrow(df))
  smooth[outside]    <- cdf[iter_outside]
  smooth[!outside]   <- 0

  df$smooth_iter <- smooth
  df
}
