#' Add information about confidence interval
#'
#' @description
#'
#' `shade_confidence_interval()` plots a confidence interval region on top of
#' [visualize()] output. The output is a ggplot2 layer that can be added with
#' `+`. The function has a shorter alias, `shade_ci()`.
#' 
#' Learn more in `vignette("infer")`.
#'
#' @param endpoints The lower and upper bounds of the interval to be plotted.
#'   Likely, this will be the output of [get_confidence_interval()].
#'   For [calculate()]-based workflows, this will be a 2-element vector 
#'   or a `1 x 2` data frame containing the lower and upper values to be plotted. 
#'   For [`fit()`][fit.infer()]-based workflows, a `(p + 1) x 3` data frame
#'   with columns `term`, `lower_ci`, and `upper_ci`, giving the upper and
#'   lower bounds for each regression term. For use in visualizations of
#'   [assume()] output, this must be the output of [get_confidence_interval()].
#' @param color A character or hex string specifying the color of the
#'   end points as a vertical lines on the plot.
#' @param fill A character or hex string specifying the color to shade the
#'   confidence interval. If `NULL` then no shading is actually done.
#' @param ... Other arguments passed along to \\{ggplot2\\} functions.
#' 
#' @return If added to an existing {infer} visualization, a \\{ggplot2\\} 
#'   object displaying the supplied intervals on top of its corresponding
#'   distribution. Otherwise, an `infer_layer` list.
#'
#' @examples
#' # find the point estimate---mean number of hours worked per week
#' point_estimate <- gss %>%
#'   specify(response = hours) %>%
#'   calculate(stat = "mean")
#'   
#' # ...and a bootstrap distribution
#' boot_dist <- gss %>%
#'   # ...we're interested in the number of hours worked per week
#'   specify(response = hours) %>%
#'   # generating data points
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   # finding the distribution from the generated data
#'   calculate(stat = "mean")
#'   
#' # find a confidence interval around the point estimate
#' ci <- boot_dist %>%
#'   get_confidence_interval(point_estimate = point_estimate,
#'                           # at the 95% confidence level
#'                           level = .95,
#'                           # using the standard error method
#'                           type = "se")   
#'   
#'   
#' # and plot it!
#' boot_dist %>%
#'   visualize() +
#'   shade_confidence_interval(ci)
#'   
#' # or just plot the bounds
#' boot_dist %>%
#'   visualize() +
#'   shade_confidence_interval(ci, fill = NULL)
#'   
#' # you can shade confidence intervals on top of
#' # theoretical distributions, too---the theoretical
#' # distribution will be recentered and rescaled to
#' # align with the confidence interval
#' sampling_dist <- gss %>%
#'   specify(response = hours) %>%
#'   assume(distribution = "t") 
#'   
#' visualize(sampling_dist) +
#'   shade_confidence_interval(ci)
#'
#' \donttest{
#' # to visualize distributions of coefficients for multiple
#' # explanatory variables, use a `fit()`-based workflow
#' 
#' # fit 1000 linear models with the `hours` variable permuted
#' null_fits <- gss %>%
#'  specify(hours ~ age + college) %>%
#'  hypothesize(null = "independence") %>%
#'  generate(reps = 1000, type = "permute") %>%
#'  fit()
#'  
#' null_fits
#' 
#' # fit a linear model to the observed data
#' obs_fit <- gss %>%
#'   specify(hours ~ age + college) %>%
#'   fit()
#'
#' obs_fit
#' 
#' # get confidence intervals for each term
#' conf_ints <- 
#'   get_confidence_interval(
#'     null_fits, 
#'     point_estimate = obs_fit, 
#'     level = .95
#'   )
#' 
#' # visualize distributions of coefficients 
#' # generated under the null
#' visualize(null_fits)
#' 
#' # add a confidence interval shading layer to juxtapose 
#' # the null fits with the observed fit for each term
#' visualize(null_fits) + 
#'   shade_confidence_interval(conf_ints)
#' }
#'
#' # more in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @name shade_confidence_interval
NULL

#' @rdname shade_confidence_interval
#' @family visualization functions
#' @export
shade_confidence_interval <- function(endpoints, color = "mediumaquamarine",
                                      fill = "turquoise", ...) {
  # since most of the logic for shading is in shade_confidence_interval_term, which 
  # is only called by `+.gg`, we need to check for mistakenly piped inputs here
  check_for_piped_visualize(endpoints, color, fill)
  
  # store inputs in classed output that can passed to a `ggplot_add` method
  structure(
    "A confidence interval shading layer.", 
    class = "infer_layer",
    fn = "shade_confidence_interval",
    endpoints = if (is.null(endpoints)) {NA} else {endpoints},
    color = color,
    fill = fill,
    dots = list(...)
  )
}

shade_confidence_interval_term <- function(plot, endpoints, 
                                           color = "mediumaquamarine",
                                           fill = "turquoise", dots) {
  if (all(is.na(endpoints))) {
    endpoints <- NULL
  }
  
  # argument checking
  endpoints <- impute_endpoints(endpoints, plot)
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
    list(linewidth = 2)
  )
  segment_layer <- do.call(ggplot2::geom_segment, segment_args)
  
  res <- c(res, list(segment_layer))
  
  if (inherits(plot[["plot_env"]][["data"]], "infer_dist")) {
    plot <- 
      redraw_theory_layer(
        plot, 
        mean_shift = attr(endpoints, "point_estimate"),
        sd_shift = attr(endpoints, "se")
      ) +
      ggplot2::labs(
        title = "Rescaled Theoretical Distribution",
        x = "stat"
      )
  }

  plot + res
}

#' @rdname shade_confidence_interval
#' @export
shade_ci <- shade_confidence_interval
