#' Add information about p-value region(s)
#'
#' `shade_p_value()` plots p-value region(s) on top of the [visualize()] output.
#' It should be used as \\{ggplot2\\} layer function (see examples).
#' `shade_pvalue()` is its alias.
#'
#' @param obs_stat A numeric value or 1x1 data frame corresponding to what the
#'   observed statistic is.
#' @param direction A string specifying in which direction the shading should
#'   occur. Options are `"less"`, `"greater"`, or `"two_sided"`. Can
#'   also give `"left"`, `"right"`, or `"both"`. If `NULL` then no shading is
#'   actually done.
#' @param color A character or hex string specifying the color of the observed
#'   statistic as a vertical line on the plot.
#' @param fill A character or hex string specifying the color to shade the
#'   p-value region. If `NULL` then no shading is actually done.
#' @param ... Other arguments passed along to \\{ggplot2\\} functions.
#'
#' @return A list of \\{ggplot2\\} objects to be added to the `visualize()`
#'   output.
#'
#' @seealso [shade_confidence_interval()] to add information about confidence
#'   interval.
#'
#' @examples
#' set.seed(505)
#' data_to_plot <- mtcars %>%
#'   dplyr::mutate(am = factor(am)) %>%
#'   specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 100, type = "permute") %>%
#'   calculate(stat = "t", order = c("1", "0"))
#'
#' visualize(data_to_plot, method = "simulation") +
#'   shade_p_value(1.5, direction = "right")
#' visualize(data_to_plot, method = "theoretical") +
#'   shade_p_value(1.5, direction = "left")
#' visualize(data_to_plot, method = "both") +
#'   shade_p_value(1.5, direction = "both")
#' visualize(data_to_plot) + shade_p_value(1.5, direction = NULL)
#'
#' @name shade_p_value
NULL

#' @rdname shade_p_value
#' @export
shade_p_value <- function(obs_stat, direction,
                          color = "red2", fill = "pink", ...) {
  obs_stat <- check_obs_stat(obs_stat)
  check_shade_p_value_args(obs_stat, direction, color, fill)
  
  res <- list()
  if (is.null(obs_stat)) {
    return(res)
  }
  
  # Add shading
  if (!is.null(direction) && !is.null(fill)) {
    if (direction %in% c("less", "left", "greater", "right")) {
      tail_area <- one_tail_area(obs_stat, direction)
      
      res <- c(res, geom_tail_area(tail_area, fill))
    } else if (direction %in% c("two_sided", "both")) {
      tail_area <- two_tail_area(obs_stat, direction)
      
      res <- c(res, geom_tail_area(tail_area, fill))
    } else {
      warning_glue(
        '`direction` should be one of `"less"`, `"left"`, `"greater"`, ",
        "`"right"`, `"two_sided"`, `"both"`.'
      )
    }
  }
  
  # Add vertical line at `obs_stat`
  c(
    res,
    list(ggplot2::geom_segment(
      # Here `aes()` is needed to force {ggplot2} to include segment in the plot
      aes(x = obs_stat, xend = obs_stat, y = 0, yend = Inf),
      colour = color, size = 2,
      inherit.aes = FALSE
    ))
  )
}

#' @rdname shade_p_value
#' @export
shade_pvalue <- shade_p_value

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

geom_tail_area <- function(tail_data, fill) {
  list(
    ggplot2::geom_area(
      data = tail_data, mapping = aes(x = x, y = y, group = dir),
      fill = fill, alpha = 0.6,
      show.legend = FALSE, inherit.aes = FALSE
    )
  )
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
  g <- ggplot(data) + theoretical_layer(data, "black", do_warn = FALSE)
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
    two_sided = , both = "both"
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
