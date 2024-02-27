#' Plot a given list of time series.
#'
#' Given a list of time series and a list of labels, this function will plot the
#' series and assign a label to each one of them. The first time serie will be
#' labeled with the first label and so on.
#'
#' The number of time series in
#' `time_series_list` is required to be the same as the number of labels in
#' `labels`. Besides, each time serie is required to have both `x` and `y`
#' columns, that will be used to plot them.
#'
#' @param time_series_list list List of time series tibbles to be plotted
#' @param labels list The list of labels to each time serie. The first time serie
#'   of the `time_series_list` will be labeled by the first label in this
#'   parameter and so on.
#' @param theme A theme object from ggplot. Default is theme_bw()
#' @return The plot object.
#'
#' @examples
#' \dontrun{
#' ts1 <- tibble(x = 1:100, y = cumsum(rnorm(100)))
#' ts2 <- tibble(x = 1:100, y = cumsum(rnorm(100)))
#' ts3 <- tibble(x = 1:100, y = cumsum(rnorm(100)))
#'
#' # Create a list of time series tibbles and labels
#' time_series_tibbles <- list(ts1, ts2, ts3)
#' labels <- c("Label A", "Label B", "Label C")
#'
#' plot_time_series(time_series_tibbles, labels)
#' }
#' @export
#' @import tidyverse
plot_time_series <- function(time_series_list, labels, theme = theme_bw()) {

  if (length(time_series_list) != length(labels)) {
    stop("Length of `time_series_list` and `labels` must be the same.")
  }

  # Combine time series and labels into a single tibble
  names(time_series_list) <- labels
  binded_df <- bind_rows(time_series_list, .id = "label")

  # Check wether the combined dataframe has the `x` and `y` columns or not
  if (!all(c("x", "y") %in% colnames(binded_df))) {
    stop("All datasets must have `x` and `y` columns.")
  }
  # Plot using ggplot2
  return(ggplot(binded_df, aes(x = x, y = y, color = label)) +
    geom_line() +
    labs(x = "Time", y = "Value") +
    theme +
    scale_color_manual(values = rainbow(length(unique(labels)))) +
    guides(color = guide_legend(title = "Labels")))
}
