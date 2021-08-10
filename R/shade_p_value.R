#' Shade histogram area beyond an observed statistic
#'
#' @description
#'
#' `shade_p_value()` plots a p-value region on top of
#' [visualize()] output. The output is a ggplot2 layer that can be added with
#' `+`. The function has a shorter alias, `shade_pvalue()`.
#' 
#' Learn more in `vignette("infer")`.
#'
#' @param obs_stat The observed statistic or estimate. For 
#'   [calculate()]-based workflows, this will be a 1-element numeric vector or 
#'   a `1 x 1` data frame containing the observed statistic. 
#'   For [`fit()`][fit.infer()]-based workflows, a `(p + 1) x 2` data frame 
#'   with columns `term` and `estimate` giving the observed estimate for
#'   each term.
#' @param direction A string specifying in which direction the shading should
#'   occur. Options are `"less"`, `"greater"`, or `"two-sided"`. Can
#'   also give `"left"`, `"right"`, `"both"`, `"two_sided"`, `"two sided"`,
#'   or `"two.sided"`. If `NULL`, the function will not shade any area.
#' @param color A character or hex string specifying the color of the observed
#'   statistic as a vertical line on the plot.
#' @param fill A character or hex string specifying the color to shade the
#'   p-value region. If `NULL`, the function will not shade any area.
#' @param ... Other arguments passed along to \\{ggplot2\\} functions.
#'   For expert use only.
#'
#' @return If added to an existing {infer} visualization, a \\{ggplot2\\} 
#'   object displaying the supplied statistic on top of its corresponding
#'   distribution. Otherwise, an `infer_layer` list.
#'
#'
#' @examples
#' # find the point estimate---mean number of hours worked per week
#' point_estimate <- gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40) %>%
#'   calculate(stat = "t")
#'   
#' # ...and a null distribution
#' null_dist <- gss %>%
#'   # ...we're interested in the number of hours worked per week
#'   specify(response = hours) %>%
#'   # hypothesizing that the mean is 40
#'   hypothesize(null = "point", mu = 40) %>%
#'   # generating data points for a null distribution
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   # estimating the null distribution
#'   calculate(stat = "t")
#'   
#' # shade the p-value of the point estimate
#' null_dist %>%
#'   visualize() +
#'   shade_p_value(obs_stat = point_estimate, direction = "two-sided")
#'   
#' # you can shade confidence intervals on top of
#' # theoretical distributions, too!
#' null_dist_theory <- gss %>%
#'   specify(response = hours) %>%
#'   assume(distribution = "t") 
#'   
#' null_dist_theory %>%
#'   visualize() +
#'   shade_p_value(obs_stat = point_estimate, direction = "two-sided")
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
#' # visualize distributions of coefficients 
#' # generated under the null
#' visualize(null_fits)
#' 
#' # add a p-value shading layer to juxtapose the null 
#' # fits with the observed fit for each term
#' visualize(null_fits) + 
#'   shade_p_value(obs_fit, direction = "both")
#' 
#' # the direction argument will be applied 
#' # to the plot for each term
#' visualize(null_fits) + 
#'   shade_p_value(obs_fit, direction = "left")
#' }
#' 
#' # more in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @name shade_p_value
NULL

#' @rdname shade_p_value
#' @family visualization functions
#' @export
shade_p_value <- function(obs_stat, direction,
                          color = "red2", fill = "pink", ...) {
  # since most of the logic for p-value shading is in shade_p_value_term, which 
  # is only called by `+.gg`, we need to check for mistakenly piped inputs here
  check_for_piped_visualize(obs_stat, direction, color, fill)
  
  # store inputs in classed output that can passed to a `ggplot_add` method
  structure(
    "A p-value shading layer.", 
    class = "infer_layer",
    fn = "shade_p_value",
    obs_stat = if (is.null(obs_stat)) {NA} else {obs_stat},
    direction = if (is.null(direction)) {NA} else {direction},
    color = color,
    fill = fill,
    dots = list(...)
  )
}

#' @rdname shade_p_value
#' @export
shade_pvalue <- shade_p_value

shade_p_value_term <- function(plot, obs_stat, direction,
                               color = "red2", fill = "pink", dots) {
  if (all(is.na(obs_stat))) {
    obs_stat <- NULL
  }
  
  if (all(is.na(direction))) {
    direction <- NULL
  }
  
  # argument checking
  obs_stat <- check_obs_stat(obs_stat, plot)
  check_shade_p_value_args(obs_stat, direction, color, fill)
  
  term <- x_axis_label(plot)
  
  res <- list()
  if (is.null(obs_stat)) {
    return(res)
  }
  
  # Add shading
  if (!is.null(direction) && !is.null(fill)) {
    if (direction %in% c("less", "left", "greater", "right")) {
      tail_area <- one_tail_area(obs_stat, direction)
      
      res <- c(res, do.call(geom_tail_area, c(list(tail_area, fill), dots)))
    } else if (direction %in% c("two_sided", "both", 
                                "two-sided", "two sided", "two.sided")) {
      tail_area <- two_tail_area(obs_stat, direction)
      
      res <- c(res, do.call(geom_tail_area, c(list(tail_area, fill), dots)))
    } else {
      warning_glue(
        '`direction` should be one of `"less"`, `"left"`, `"greater"`, ',
        '`"right"`, `"two-sided"`, `"both"`, `"two_sided"`, `"two sided"`, ',
        'or `"two.sided"`.'
      )
    }
  }
  
  # Add vertical line at `obs_stat`
  # Making extra step of precomputing arguments in order to have default value
  # of `size = 2` overwritable in `...`
  segment_args <- c_dedupl(
    # Not overwritable arguments
    list(
      # Here `aes()` is needed to force {ggplot2} to include segment in the plot
      mapping = aes(x = obs_stat, xend = obs_stat, y = 0, yend = Inf),
      color = color,
      inherit.aes = FALSE
    ),
    # Extra arguments
    dots,
    # Default arguments that might be replaced in `...`
    list(size = 2)
  )
  segment_layer <- do.call(ggplot2::geom_segment, segment_args)
  
  res <- c(res, list(segment_layer))
  
  plot + res
}


check_shade_p_value_args <- function(obs_stat, direction, color, fill) {
  if (!is.null(obs_stat)) {
    check_type(obs_stat, is.numeric)
  }
  if (!is.null(direction)) {
    check_type(direction, is.character)
  }
  check_type(color, is_color_string, "color string")
  check_type(fill, is_color_string, "color string")
  
  TRUE
}

geom_tail_area <- function(tail_data, fill, ...) {
  area_args <- c_dedupl(
    list(
      data = tail_data,
      mapping = aes(x = x, y = y, group = dir),
      fill = fill,
      show.legend = FALSE,
      inherit.aes = FALSE
    ),
    list(...),
    list(alpha = 0.6)
  )
  area_layer <- do.call(ggplot2::geom_area, area_args)
  
  list(area_layer)
}

two_tail_area <- function(obs_stat, direction) {
  # Take advantage of {ggplot2} functionality to accept function as `data`.
  # This is needed to make possible existence of `shade_p_value()` in case of
  # `direction = "both"`, as it depends on actual `data` but adding it as
  # argument to `shade_p_value()` is very bad.
  # Also needed to warn about incorrect usage of right tail tests.
  function(data) {
    warn_right_tail_test(direction, short_theory_type(data))
    
    if (get_viz_method(data) == "theoretical") {
      second_border <- -obs_stat
    } else {
      second_border <- mirror_obs_stat(data$stat, obs_stat)
    }
    
    left_area <- one_tail_area(
      min(obs_stat, second_border), "left", do_warn = FALSE
    )(data)
    right_area <- one_tail_area(
      max(obs_stat, second_border), "right", do_warn = FALSE
    )(data)
    
    dplyr::bind_rows(left_area, right_area)
  }
}

one_tail_area <- function(obs_stat, direction, do_warn = TRUE) {
  # Take advantage of {ggplot2} functionality to accept function as `data`.
  function(data) {
    warn_right_tail_test(direction, short_theory_type(data), do_warn)
    
    norm_dir <- norm_direction(direction)
    viz_method <- get_viz_method(data)
    
    # Compute grid points for upper bound of shading area
    switch(
      viz_method,
      theoretical = theor_area(data, obs_stat, norm_dir),
      simulation  = hist_area(data, obs_stat, norm_dir, yval = "ymax"),
      both        = hist_area(data, obs_stat, norm_dir, yval = "density")
    )
  }
}

theor_area <- function(data, obs_stat, direction, n_grid = 1001) {
  plot_data <- create_plot_data(data)
  
  g <- ggplot(plot_data) + theoretical_layer(data, "black", do_warn = FALSE)
  g_data <- ggplot2::ggplot_build(g)[["data"]][[1]]
  
  curve_fun <- stats::approxfun(
    x = g_data[["x"]], y = g_data[["y"]], yleft = 0, yright = 0
  )
  
  # Compute "x" grid of curve, area under which will be shaded.
  x_grid <- switch(
    # `direction` can be one of "left" or "right" at this point of execution
    direction,
    left  = seq(from = min(g_data[["x"]]), to = obs_stat, length.out = n_grid),
    right = seq(from = obs_stat, to = max(g_data[["x"]]), length.out = n_grid)
  )
  
  tibble::tibble(x = x_grid, y = curve_fun(x_grid), dir = direction)
}

hist_area <- function(data, obs_stat, direction, yval) {
  g <- ggplot(data) + simulation_layer(data)
  g_data <- ggplot2::ggplot_build(g)[["data"]][[1]]
  
  # Compute knots for step function representing histogram bars and space
  # between them.
  # "x" coordinates are computed from `x_left` and `x_right`: "x" coordinates
  # of "shrinked" (to avoid duplicte points later) histogram bars.
  x_left <- (1-1e-5)*g_data[["xmin"]] + 1e-5*g_data[["xmax"]]
  x_right <- 1e-5*g_data[["xmin"]] + (1 - 1e-5)*g_data[["xmax"]]
  # `x` is created as `c(x_left[1], x_right[1], x_left[2], ...)`
  x <- c(t(cbind(x_left, x_right)))

  # "y" coordinates represent values of future `stepfun(..., right = FALSE)`
  # outputs between `x` knots. That is:
  # y[1] is value inside [-Inf, x_left[1]) (zero),
  # y[2] - value inside [x_left[1], x_right[1]) (height of first histogram bar),
  # y[3] - value inside [x_right[1], x_left[2]) (zero), and so on.
  y <- c(0, t(cbind(g_data[[yval]], 0)))
  
  # Output step function should evaluate to histogram bar heights on both
  # corresponding ends, i.e. `curve_fun(c(x_left[1], x_right[1]))` should return
  # vector of length two with heights of first histogram bar. `stepfun()` treats
  # input `x` as consequtive semi-open intervals. To achieve effect of closed
  # intervals, `pmax()` trick is used.
  curve_fun <- function(t) {
    pmax(
      stats::stepfun(x, y, right = FALSE)(t),
      stats::stepfun(x, y, right = TRUE)(t)
    )
  }
  
  # "True" left and right "x" coordinates of histogram bars are added to achieve
  # "almost vertical" lines with `geom_area()` usage. If don't do this, then
  # area might be shaded under line segments connecting edges of consequtive
  # histogram bars.
  x_extra <- sort(c(x, g_data[["xmin"]], g_data[["xmax"]]))
  x_grid <- switch(
    # `direction` can be one of "left" or "right" at this point of execution
    direction,
    left  = c(x_extra[x_extra < obs_stat], obs_stat),
    right = c(obs_stat, x_extra[x_extra > obs_stat])
  )
  
  tibble::tibble(x = x_grid, y = curve_fun(x_grid), dir = direction)
}

norm_direction <- function(direction) {
  switch(
    direction,
    less = , left = "left",
    greater = , right = "right",
    two_sided = , `two-sided` = , `two sided` = , `two.sided` = , both = "both"
  )
}

warn_right_tail_test <- function(direction, stat_name, do_warn = TRUE) {
  if (do_warn && !is.null(direction) &&
      !(direction %in% c("greater", "right")) &&
      (stat_name %in% c("F", "Chi-Square"))) {
    warning_glue(
      "{stat_name} usually corresponds to right-tailed tests. ",
      "Proceed with caution."
    )
  }
  
  TRUE
}

mirror_obs_stat <- function(vector, observation) {
  obs_percentile <- stats::ecdf(vector)(observation)
  
  stats::quantile(vector, probs = 1 - obs_percentile)
}

