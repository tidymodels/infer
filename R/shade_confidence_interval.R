#' Add information about confidence interval
#'
#' @description
#'
#' `shade_confidence_interval()` plots confidence interval region on top of the
#' [visualize()] output. It should be used as \\{ggplot2\\} layer function (see
#' examples). `shade_ci()` is its alias.
#' 
#' Learn more in `vignette("infer")`.
#'
#' @param endpoints A 2 element vector or a 1 x 2 data frame containing the
#'   lower and upper values to be plotted. Most useful for visualizing
#'   conference intervals.
#' @param color A character or hex string specifying the color of the
#'   end points as a vertical lines on the plot.
#' @param fill A character or hex string specifying the color to shade the
#'   confidence interval. If `NULL` then no shading is actually done.
#' @param ... Other arguments passed along to \\{ggplot2\\} functions.
#' 
#' @return A list of \\{ggplot2\\} objects to be added to the `visualize()`
#'   output.
#'
#' @seealso [shade_p_value()] to add information about p-value region.
#'
#' @examples
#' # find the point estimate---mean number of hours worked per week
#' point_estimate <- gss %>%
#'   specify(response = hours) %>%
#'   calculate(stat = "mean") %>%
#'   dplyr::pull()
#'   
#' # ...and a null distribution
#' null_dist <- gss %>%
#'   # ...we're interested in the number of hours worked per week
#'   specify(response = hours) %>%
#'   # hypothesizing that the mean is 40
#'   hypothesize(null = "point", mu = 40) %>%
#'   # generating data points for a null distribution
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   # finding the null distribution
#'   calculate(stat = "mean")
#'   
#' # find a confidence interval around the point estimate
#' ci <- null_dist %>%
#'   get_confidence_interval(point_estimate = point_estimate,
#'                           # at the 95% confidence level
#'                           level = .95,
#'                           # using the standard error method
#'                           type = "se")   
#'   
#'   
#' # and plot it!
#' null_dist %>%
#'   visualize() +
#'   shade_confidence_interval(ci)
#'   
#' # or just plot the bounds
#' null_dist %>%
#'   visualize() +
#'   shade_confidence_interval(ci, fill = NULL)
#'
#' # More in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @name shade_confidence_interval
NULL

#' @rdname shade_confidence_interval
#' @export
shade_confidence_interval <- function(endpoints, color = "mediumaquamarine",
                                      fill = "turquoise", ...) {
  # argument checking
  check_for_piped_visualize(endpoints, color, fill)
  
  dots <- list(...)
  
  endpoints <- impute_endpoints(endpoints)
  check_shade_confidence_interval_args(color, fill)
  
  res <- list()
  if (is.null(endpoints)) {
    return(res)
  }
  
  if (!is.null(fill)) {
    # Making extra step of precomputing arguments in order to have default value
    # of `alpha = 0.6` overwritable in `...`
    rect_args <- c_dedupl(
      # Not overwritable arguments
      list(
        data = data.frame(endpoints[1]),
        mapping = aes(
          xmin = endpoints[1], xmax = endpoints[2], ymin = 0, ymax = Inf
        ),
        fill = fill,
        inherit.aes = FALSE
      ),
      # Extra arguments
      dots,
      # Default arguments that might be replaced in `...`
      list(alpha = 0.6)
    )
    rect_layer <- do.call(ggplot2::geom_rect, rect_args)
    
    res <- c(res, list(rect_layer))
  }
  
  segment_args <- c_dedupl(
    list(
      data = data.frame(x = endpoints),
      mapping = aes(x = x, xend = x, y = 0, yend = Inf),
      color = color,
      inherit.aes = FALSE
    ),
    dots,
    list(size = 2)
  )
  segment_layer <- do.call(ggplot2::geom_segment, segment_args)
  
  c(res, list(segment_layer))
}

#' @rdname shade_confidence_interval
#' @export
shade_ci <- shade_confidence_interval
