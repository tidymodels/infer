#' (Currently) Visualize the randomization-based distribution 
#' (To be updated to include theory-based distributions)
#' @param df the output from \code{\link{calculate}}
#' @param bins the number of bins in the histogram
#' @param ... currently ignored
#' @return A ggplot object showing the randomization-based distribution as a histogram.
#' Preferable to use the ggplot2 package directly here as a histogram does not always
#' display the distribution well.
#' @importFrom ggplot2 ggplot geom_histogram aes
#' @export

visualize <- function(df, bins = 30, ...) {
  # TODO:  determine whether a bar graph or a histogram is
  # more appropriate
  
  assertive::assert_is_data.frame(df)
  assertive::assert_is_numeric(bins)
  
  ggplot(data = df, mapping = aes(x = stat)) +
    geom_histogram(bins = bins, color = "white")
}
