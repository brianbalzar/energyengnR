# ----------
# Author: Brian Balzar
# Title: bin.r
# Description: Bins data in a data frame
# ----------



#' bin_data
#'
#' @param df Data frame. Must contain a column to bin and a column to categorize.
#' @param bin The name of the column to bin.
#' @param cat The name of the column to summarize.
#' @param func Function to use in summarization.
#' @param n Maximum number of breaks.
#' @param ... Paramters to pass to bin.
#'
#' @return A binned data frame
#' @export
#'
#' @examples
#'
#' given = data_frame(
#'   x = c(0, 35, 60, 85),
#'   y = c(250, 500, 1000, 1500)
#' )
#'
#' mdl <- lm(y ~ poly(x, degree = 3, raw = TRUE), data = given)
#'
#' dummy <- data_frame(
#'   oat = ceiling(runif(1000, 20, 95)),
#'   tons = predict(mdl, data.frame(x = oat)) + runif(1000, -10, 10)
#' )
#'
#' binned <- bin_data(dummy, oat, tons, n = 20)
#'
#'
bin_data <- function(df, bin, cat, func = mean, n = 10, ...) {

  func <- match.fun(func)
  bin_quo <- dplyr::enquo(bin)
  cat_quo <- dplyr::enquo(cat)

  bin_subs <- deparse(substitute(bin))
  max_bin <- max(df[, bin_subs], na.rm = TRUE)
  min_bin <- min(df[, bin_subs], na.rm = TRUE)

  breaks <- get_breaks(min_bin, max_bin, n)
  labels <- get_labels(breaks)

  play <- df %>%
    mutate(bin = cut(!!bin_quo, breaks = breaks, labels = labels))

  counts <- play %>%
    group_by(bin) %>%
    summarize(count = n())

  int <- play %>%
    group_by(bin) %>%
    summarize_at(vars(!!cat_quo), func, na.rm = TRUE)

  out <- counts %>%
    full_join(int, by = "bin")

  return(out)

}
