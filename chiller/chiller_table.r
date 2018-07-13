#-----------
# Title: chiller_table.r
# Author: Brian Balzar
# Description: Produces a formattable to analyze a chiller plant's operation
#-----------

#' table_scenario
#'
#' Creates a formmattable with bin data by scenario. In the background is a
#' two-color bar which represents the quantity of data points for each scenario
#' in that bin.
#'
#' @param df The data frame. Must contain a named "scenario" with factors. Also
#' should contain columns to bin and value.
#'
#' @param bin The name of the column to bin (i.e. tons)
#' @param val The name of the value to summarize (i.e. eff)
#' @param n The maximum number of breaks to use for bin data
#' @param back_color The back color of the bar.
#' @param prop_color The color used to represent the proportion of each
#' scenario.
#' @param digits Number of digits to display in each box.
#' @param ... Parameters to pass through
#'
#' @return A formattable object
#' @export
#'
#' @examples
#'
#' dummy <- data_frame(
#' tons = rnorm(8760, mean = 1500, sd = 500),
#' scenario = sample(letters[1:5], 8760, replace = TRUE, prob = c(1, 3, 7, 3, 5)),
#' eff = rnorm(8760, mean = 0.55, sd = 0.25),
#' temp = rnorm(8760, mean = 70, sd = 10)
#'  ) %>%
#'   mutate(
#'     tons = ifelse(tons < 0, 0, tons),
#'     eff = ifelse(eff < 0, 0, eff)
#'   )
#'
#'  d <- table_scenario(dummy, tons, eff)

table_scenario <- function(df, bin, val, n = 10, back_color = "lightgray",
                           prop_color = "skyblue2", digits = 3, ...) {


  bin_subs <- deparse(substitute(bin))
  bin_quo <- dplyr::enquo(bin)
  val_quo <- dplyr::enquo(val)

  max_bin <- max(df[, bin_subs], na.rm = TRUE)
  min_bin <- min(df[, bin_subs], na.rm = TRUE)
  breaks <- get_breaks(min_bin, max_bin, n)
  labels <- get_labels(breaks)
  n_scenario <- length(unique(df$scenario[df$scenario != ""]))

  df_cut <- df %>%
    mutate(bucket = cut(!!bin_quo, breaks = breaks, labels = labels))

  avs <- df_cut %>%
    group_by(bucket) %>%
    summarize(val = signif(mean(!!val_quo, na.rm = TRUE), 3)) %>%
    na.omit() %>%
    spread(bucket, val) %>%
    mutate(scenario = "Average")

  cnts <- df_cut %>%
    group_by(bucket) %>%
    summarize(count = n()) %>%
    na.omit() %>%
    spread(bucket, count) %>%
    mutate(scenario = "Counts")

  scen_bin <- df_cut %>%
    group_by(bucket, scenario) %>%
    summarize(count = n(),
              value = mean(!!val_quo, na.rm = TRUE)) %>%
    na.omit() %>%
    mutate(prop = count / sum(count),
           fmt = paste(value, prop, sep = "_")) %>%
    select(-c(count, value, prop)) %>%
    spread(bucket, fmt) %>%
    rbind(avs, cnts) %>%
    formattable(list(
      area(row = 1:n_scenario, col = 2:length(breaks)) ~
                       .formatter_type(back_color = back_color,
                                      prop_color = prop_color,
                                      digits = digits))) %>%
    as.htmlwidget()

}

# get_breaks and get_labels are needed for bin.r
get_breaks <- function(min, max, max_n) {
  diff <- max - min
  steps <- c(1, 5, 10, 20, 25, 50, 100, 250, 500)
  interval <- function(x) steps[ifelse(x < min(steps), 1, findInterval(x, steps))]
  interv <- interval(2 * diff/max_n)
  lower <- floor(min/interv) * interv
  upper <- ceiling(max/interv) * interv
  out <- seq(lower, upper, interv)
}

get_labels <- function(breaks, sep = "\n") {
  out <- paste(seq(breaks[1], breaks[length(breaks -1)], breaks[2] - breaks[1]),
               seq(breaks[2], breaks[length(breaks)], breaks[2] - breaks[1]),
               sep = sep) %>%
    head(-1)
}

#' .formatter_type
#'
#' Creates a two-color proportion bar to be used in formattables
#'
#' @param back_color The back color of the bar.
#' @param prop_color The proportion color of the bar.
#' @param fun Display type (comma, currency, etc.)
#' @param digits Number of digits to display
#'
#' @return A formatter type.
#' @export
#'
#' @examples
.formatter_type <- function(back_color = "lightgray", prop_color = "skyblue2",
                            fun = "comma", digits = 0) {

  back_color <- csscolor(col2rgb(back_color))
  prop_color <- csscolor(col2rgb(prop_color))
  fun <- match.fun(fun)
  formatter("span", x ~ fun(.extract_type(x, "_", 1), digits = digits),
            style = function(y) style(
              display = "inline-block",
              direction = "ltr",
              "border-radius" = "4px",
              "padding-right" = "2px",
              "background-color" = csscolor(prop_color),
              "background-image" = glue::glue("linear-gradient({back_color},
                                              {back_color})"),
              "background-repeat" = "no-repeat",
              "background-size" = paste(1 - percent(.extract_type(y, "_", 2)),
                                        " 100%", sep = ""),
              width = "100%",
              "background-position" = 0
            )
  )
}

.extract_type <- function(str, pat, n, size = 2) {
  out <- stringr::str_split_fixed(str, pat, size)[, n] %>%
    as.double()
}
