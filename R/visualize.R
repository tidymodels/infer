#' (Currently) Visualize the resampling distribution 
#' (To be updated to include theory-based distributions)
#' @param df the output from \code{\link{calculate}}
#' @param num_bins the number of bins in the histogram
#' @param ... currently ignored
#' @importFrom ggplot2 ggplot geom_histogram
#' @export
#' @examples
#'

visualize <- function(df, num_bins = 30, ...) {
  ggplot(data = df, mapping = aes(x = stat)) +
    geom_histogram(bins = num_bins, color = "white")
}