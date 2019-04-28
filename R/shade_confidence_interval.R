#' Add information about confidence interval
#'
#' `shade_confidence_interval()` plots confidence interval region on top of the
#' [visualize()] output. It should be used as \\{ggplot2\\} layer function (see
#' examples). `shade_ci()` is its alias.
#'
#' @param endpoints A 2 element vector or a 1 x 2 data frame containing the
#'   lower and upper values to be plotted. Most useful for visualizing
#'   conference intervals.
#' @param color A character or hex string specifying the color of the
#'   end points as a vertical lines on the plot.
#' @param fill A character or hex string specifying the color to shade the
#'   confidence interval. If `NULL` then no shading is actually done.
#' @param ... Other arguments passed along to \\{ggplot2\\} functions.
#' @return A list of \\{ggplot2\\} objects to be added to the `visualize()`
#'   output.
#'
#' @seealso [shade_p_value()] to add information about p-value region.
#'
#' @examples
#' viz_plot <- mtcars %>%
#'   dplyr::mutate(am = factor(am)) %>%
#'   specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 100, type = "permute") %>%
#'   calculate(stat = "t", order = c("1", "0")) %>%
#'   visualize(method = "both")
#'
#' viz_plot + shade_confidence_interval(c(-1.5, 1.5))
#' viz_plot + shade_confidence_interval(c(-1.5, 1.5), fill = NULL)
#'
#' @name shade_confidence_interval
NULL

#' @rdname shade_confidence_interval
#' @export
shade_confidence_interval <- function(endpoints, color = "mediumaquamarine",
                                      fill = "turquoise", ...) {
  endpoints <- impute_endpoints(endpoints)
  check_shade_confidence_interval_args(color, fill)
  
  res <- list()
  if (is.null(endpoints)) {
    return(res)
  }
  
  if (!is.null(fill)) {
    res <- c(
      res, list(
        ggplot2::geom_rect(
          data = data.frame(endpoints[1]),
          fill = fill, alpha = 0.6,
          aes(xmin = endpoints[1], xmax = endpoints[2], ymin = 0, ymax = Inf),
          inherit.aes = FALSE,
          ...
        )
      )
    )
  }
  
  c(
    res,
    list(
      ggplot2::geom_segment(
        data = data.frame(x = endpoints),
        aes(x = x, xend = x, y = 0, yend = Inf),
        colour = color, size = 2,
        inherit.aes = FALSE
      )
    )
  )
}

#' @rdname shade_confidence_interval
#' @export
shade_ci <- shade_confidence_interval
