#' (Currently) Visualize the resampling distribution 
#' (To be updated to include theory-based distributions)
#' @param df the output from \code{\link{calculate}}
#' @param bins the number of bins in the histogram
#' @param ... currently ignored
#' @importFrom ggplot2 ggplot geom_histogram aes
#' @export

visualize <- function(df, bins = 30, ...) {
  # TODO:  determine whether a bar graph or a histogram is
  # more appropriate
  ggplot(data = df, mapping = aes(x = stat)) +
    geom_histogram(bins = bins, color = "white")
}