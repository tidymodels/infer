#' Visualize statistical inference
#'
#' Visualize the distribution of the simulation-based inferential statistics or
#' the theoretical distribution (or both!).
#'
#' @param data The output from [calculate()].
#' @param bins The number of bins in the histogram.
#' @param method A string giving the method to display. Options are
#'   `"simulation"`, `"theoretical"`, or `"both"` with `"both"` corresponding to
#'   `"simulation"` and `"theoretical"`.
#' @param dens_color A character or hex string specifying the color of the
#'   theoretical density curve.
#' @param obs_stat A numeric value or 1x1 data frame corresponding to what the
#'   observed statistic is. **Deprecated (see Details)**.
#' @param obs_stat_color A character or hex string specifying the color of the
#'   observed statistic as a vertical line on the plot. **Deprecated (see
#'   Details)**.
#' @param pvalue_fill A character or hex string specifying the color to shade
#'   the p-value. In previous versions of the package this was the `shade_color`
#'   argument. **Deprecated (see Details)**.
#' @param direction A string specifying in which direction the shading should
#'   occur. Options are `"less"`, `"greater"`, or `"two_sided"` for p-value. Can
#'   also give `"left"`, `"right"`, or `"both"` for p-value. For confidence
#'   intervals, use `"between"` and give the endpoint values in `endpoints`.
#'   **Deprecated (see Details)**.
#' @param endpoints A 2 element vector or a 1 x 2 data frame containing the
#'   lower and upper values to be plotted. Most useful for visualizing
#'   conference intervals. **Deprecated (see Details)**.
#' @param endpoints_color A character or hex string specifying the color of the
#'   observed statistic as a vertical line on the plot. **Deprecated (see
#'   Details)**.
#' @param ci_fill A character or hex string specifying the color to shade the
#'   confidence interval. **Deprecated (see Details)**.
#' @param ... Other arguments passed along to \\{ggplot2\\} functions.
#'
#' @details In order to make visualization workflow more straightforward and
#' explicit `visualize()` now only should be used to plot statistics directly.
#' That is why arguments not related to this task are deprecated and will be
#' removed in a future release of \\{infer\\}.
#' 
#' To add to plot information related to p-value use [shade_p_value()]. To add
#' to plot information related to confidence interval use
#' [shade_confidence_interval()].
#' 
#' @return A ggplot object showing the simulation-based distribution as a
#'   histogram or bar graph. Also used to show the theoretical curves.
#'
#' @seealso [shade_p_value()], [shade_confidence_interval()].
#'
#' @examples
#' # Permutations to create a simulation-based null distribution for
#' # one numerical response and one categorical predictor
#' # using t statistic
#' mtcars %>%
#'   dplyr::mutate(am = factor(am)) %>%
#'   specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 100, type = "permute") %>%
#'   calculate(stat = "t", order = c("1", "0")) %>%
#'   visualize(method = "simulation") #default method
#'
#' # Theoretical t distribution for
#' # one numerical response and one categorical predictor
#' # using t statistic
#' mtcars %>%
#'   dplyr::mutate(am = factor(am)) %>%
#'   specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'   hypothesize(null = "independence") %>%
#'   # generate() is not needed since we are not doing simulation
#'   calculate(stat = "t", order = c("1", "0")) %>%
#'   visualize(method = "theoretical")
#'
#' # Overlay theoretical distribution on top of randomized t-statistics
#' mtcars %>%
#'   dplyr::mutate(am = factor(am)) %>%
#'   specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 100, type = "permute") %>%
#'   calculate(stat = "t", order = c("1", "0")) %>%
#'   visualize(method = "both")
#'
#' @importFrom ggplot2 ggplot geom_histogram aes stat_function ggtitle
#' @importFrom ggplot2 xlab ylab geom_vline geom_rect geom_bar
#' @importFrom stats dt qt df qf dnorm qnorm dchisq qchisq
#' @export
visualize <- function(data, bins = 15, method = "simulation",
                      dens_color = "black",
                      obs_stat = NULL,
                      obs_stat_color = "red2",
                      pvalue_fill = "pink",
                      direction = NULL,
                      endpoints = NULL,
                      endpoints_color = "mediumaquamarine",
                      ci_fill = "turquoise",
                      ...) {
  check_visualize_args(
    data, bins, method, dens_color, obs_stat, obs_stat_color,
    pvalue_fill, direction, endpoints, endpoints_color, ci_fill
  )
  warn_depricated_args(obs_stat, endpoints)
  endpoints <- impute_enpoints(endpoints)
  obs_stat <- impute_obs_stat(obs_stat, direction, endpoints)
  
  # Add `method` to `data` attributes to enable later possibility of
  # complicated computation of p-value regions (in case `direction = "both"`)
  # in `shade_p_value()`.
  attr(data, "viz_method") <- method
  
  infer_plot <- ggplot(data) +
    simulation_layer(data, bins, ...) +
    theoretical_layer(data, dens_color, ...) +
    title_labels_layer(data) +
    shade_p_value(
      obs_stat, direction, obs_stat_color, pvalue_fill, ...
    )
  
  if (!is.null(direction) && (direction == "between")) {
    infer_plot <- infer_plot +
      shade_ci(endpoints, endpoints_color, ci_fill, ...)
  }
  
  infer_plot
}

#' @rdname visualize
#' @export
visualise <- visualize

check_visualize_args <- function(data, bins, method, dens_color,
                                 obs_stat, obs_stat_color,
                                 pvalue_fill, direction,
                                 endpoints, endpoints_color, ci_fill) {
  check_type(data, is.data.frame)
  check_type(bins, is.numeric)
  check_type(method, is.character)
  check_type(dens_color, is.character)
  check_type(obs_stat_color, is.character)
  check_type(pvalue_fill, is.character)
  if (!is.null(direction)) {
    check_type(direction, is.character)
  }
  if (
    is.data.frame(endpoints) &&
    ((nrow(endpoints) != 1) || (ncol(endpoints) != 2))
  ) {
    stop_glue(
      "Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector."
    )
  }
  
  if (!(method %in% c("simulation", "theoretical", "both"))) {
    stop_glue(
      'Provide `method` with one of three options: `"theoretical"`, `"both"`, ',
      'or `"simulation"`. `"simulation"` is the default.'
    )
  }
  
  if (method == "both") {
    if (!("stat" %in% names(data))) {
      stop_glue(
        '`generate()` and `calculate()` are both required to be done prior ',
        'to `visualize(method = "both")`'
      )
    }
    
    if (
      ("replicate" %in% names(data)) && (length(unique(data$replicate)) < 100)
    ) {
      warning_glue(
        "With only {length(unique(data$replicate))} replicates, it may be ",
        "difficult to see the relationship between simulation and theory."
      )
    }
  }
  
  if (!is.null(obs_stat) && !is.null(endpoints)) {
    warning_glue(
      "Values for both `endpoints` and `obs_stat` were given when only one ",
      "should be set. Ignoring `obs_stat` values."
    )
  }
  
  theory_type <- short_theory_type(data)
  if (theory_type %in% c("F", "Chi-Square")) {
    warn_right_tail_test(direction, theory_type)
  }
  
  TRUE
}

warn_depricated_args <- function(obs_stat, endpoints) {
  if (!is.null(obs_stat)) {
    warning_glue(
      "`visualize()` shouldn't be used to plot p-value. Arguments `obs_stat`, ",
      "`obs_stat_color`, `pvalue_fill`, and `direction` are deprecated. ",
      "Use `shade_p_value()` instead."
    )
  }
  
  if (!is.null(endpoints)) {
    warning_glue(
      "`visualize()` shouldn't be used to plot confidence interval. Arguments ",
      "`endpoints`, `endpoints_color`, and `ci_fill` are deprecated. ",
      "Use `shade_confidence_interval()` instead."
    )
  }
  
  TRUE
}

impute_enpoints <- function(endpoints) {
  if (is.vector(endpoints) && (length(endpoints) != 2)) {
    warning_glue(
      "Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector. ",
      "Using the first two entries as the `endpoints`."
    )
    endpoints <- endpoints[1:2]
  }
  if (is.data.frame(endpoints)) {
    endpoints <- unlist(endpoints)
  }
  
  endpoints
}

impute_obs_stat <- function(obs_stat, direction, endpoints) {
  obs_stat <- check_obs_stat(obs_stat)
  
  if (
    !is.null(direction) &&
    (is.null(obs_stat) + is.null(endpoints) != 1)
  ) {
    stop_glue(
      "Shading requires either `endpoints` values for a confidence interval ",
      "or the observed statistic `obs_stat` to be provided."
    )
  }
  
  obs_stat
}

simulation_layer <- function(data, bins, ...) {
  method <- get_viz_method(data)
  
  if (method == "theoretical") {
    return(list())
  }
  
  if (method == "simulation") {
    if (length(unique(data$stat)) >= 10) {
      res <- list(
        geom_histogram(
          mapping = aes(x = stat), bins = bins, color = "white", ...
        )
      )
    } else {
      res <- list(geom_bar(mapping = aes(x = stat), ...))
    }
  } else if (method == "both") {
    res <- list(
      geom_histogram(
        mapping = aes(x = stat, y = ..density..), bins = bins,
        color = "white", ...
      )
    )
  }
  
  res
}

theoretical_layer <- function(data, dens_color, ...) {
  method <- get_viz_method(data)
  
  if (method == "simulation") {
    return(list())
  }
  
  warn_theoretical_layer(data)
  
  theory_type <- short_theory_type(data)
  
  switch(
    theory_type,
    t = theory_curve(
      method, dt, qt, list(df = attr(data, "distr_param")), dens_color
    ),
    `F` = theory_curve(
      method, df, qf,
      list(
        df1 = attr(data, "distr_param"), df2 = attr(data, "distr_param2")
      ),
      dens_color = dens_color
    ),
    z = theory_curve(method, dnorm, qnorm, list(), dens_color),
    `Chi-Square` = theory_curve(
      method, dchisq, qchisq, list(df = attr(data, "distr_param")), dens_color
    )
  )
}

warn_theoretical_layer <- function(data) {
  method <- get_viz_method(data)
  
  warning_glue(
    "Check to make sure the conditions have been met for the theoretical ",
    "method. {{infer}} currently does not check these for you."
  )
  
  if (
    !is_nuat(data, "stat") &&
    !(attr(data, "stat") %in% c("t", "z", "Chisq", "F"))
  ) {
    if (method == "theoretical") {
      warning_glue(
        "Your `calculate`d statistic and the theoretical distribution are on ",
        "different scales. Displaying only the theoretical distribution."
      )
    } else if (method == "both") {
      stop_glue(
        "Your `calculate`d statistic and the theoretical distribution are on ",
        "different scales. Use a standardized `stat` instead."
      )
    }
  }
}

theory_curve <- function(method, d_fun, q_fun, args_list, dens_color) {
  if (method == "theoretical") {
    x_range <- do.call(q_fun, c(p = list(c(0.001, 0.999)), args_list))
    
    res <- list(
      stat_function(
        data = data.frame(x = x_range), mapping = aes(x),
        fun = d_fun, args = args_list, color = dens_color
      )
    )
  } else if (method == "both") {
    res <- list(
      stat_function(
        mapping = aes(x = stat), fun = d_fun, args = args_list,
        color = dens_color
      )
    )
  }
  
  res
}

title_labels_layer <- function(data) {
  method <- get_viz_method(data)
  theory_type <- short_theory_type(data)
  
  title_string <- switch(
    method,
    simulation = "Simulation-Based Null Distribution",
    theoretical = "Theoretical {theory_type} Null Distribution",
    both = "Simulation-Based and Theoretical {theory_type} Null Distributions"
  )
  
  x_lab <- switch(method, simulation = "stat", "{theory_type} stat")
  y_lab <- switch(method, simulation = "count", "density")
  
  list(
    ggtitle(glue_null(title_string)),
    xlab(glue_null(x_lab)),
    ylab(glue_null(y_lab))
  )
}

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
#' viz_plot <- mtcars %>%
#'   dplyr::mutate(am = factor(am)) %>%
#'   specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 100, type = "permute") %>%
#'   calculate(stat = "t", order = c("1", "0")) %>%
#'   visualize(method = "both")
#' 
#' viz_plot + shade_p_value(1.5, direction = "right")
#' viz_plot + shade_p_value(1.5, direction = "both")
#' viz_plot + shade_p_value(1.5, direction = NULL)
#' 
#' @name shade_p_value
NULL

#' @rdname shade_p_value
#' @export
shade_p_value <- function(obs_stat, direction,
                          color = "red2", fill = "pink", ...) {
  res <- list()
  if (is.null(obs_stat)) {
    return(res)
  }
  
  # Add shading
  if (!is.null(direction) && !is.null(fill)) {
    if (direction %in% c("less", "left", "greater", "right")) {
      tail_data <- one_tail_data(obs_stat, direction)
      
      res <- c(res, list(geom_tail(tail_data, fill, ...)))
    } else if (direction %in% c("two_sided", "both")) {
      tail_data <- two_tail_data(obs_stat)
      
      res <- c(res, list(geom_tail(tail_data, fill, ...)))
    } else {
      warning_glue(
        '`direction` should be one of `"less"`, `"left"`, `"greater"`, ",
        "`"right"`, `"two_sided"`, `"both"`.'
      )
    }
  }
  
  # Add vertical line at `obs_stat`
  c(
    res, list(geom_vline(xintercept = obs_stat, size = 2, color = color, ...))
  )
}

#' @rdname shade_p_value
#' @export
shade_pvalue <- shade_p_value

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
  res <- list()
  if (is.null(endpoints)) {
    return(res)
  }
  
  if (!is.null(fill)) {
    res <- c(
      res, list(
        geom_rect(
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
    res, list(geom_vline(xintercept = endpoints, size = 2, color = color, ...))
  )
}

#' @rdname shade_confidence_interval
#' @export
shade_ci <- shade_confidence_interval

get_percentile <- function(vector, observation) {
  stats::ecdf(vector)(observation)
}

mirror_obs_stat <- function(vector, observation) {
  obs_percentile <- get_percentile(vector, observation)
  
  stats::quantile(vector, probs = 1 - obs_percentile)
}

short_theory_type <- function(x) {
  theory_attr <- attr(x, "theory_type")
  theory_types <- list(
    t = c("Two sample t", "Slope with t", "One sample t"),
    `F` = "ANOVA",
    z = c("One sample prop z", "Two sample props z"),
    `Chi-Square` = c("Chi-square test of indep", "Chi-square Goodness of Fit")
  )
  
  is_type <- vapply(theory_types, function(x) {theory_attr %in% x}, logical(1))
  
  names(theory_types)[which(is_type)[1]]
}

warn_right_tail_test <- function(direction, stat_name) {
  if (!is.null(direction) && !(direction %in% c("greater", "right"))) {
    warning_glue(
      "{stat_name} usually corresponds to right-tailed tests. ",
      "Proceed with caution."
    )
  }
  
  TRUE
}

geom_tail <- function(tail_data, fill, ...) {
  list(
    geom_rect(
      data = tail_data,
      aes(xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf),
      fill = fill, alpha = 0.6,
      inherit.aes = FALSE,
      ...
    )
  )
}

one_tail_data <- function(obs_stat, direction) {
  if (direction %in% c("less", "left")) {
    data.frame(x_min = -Inf, x_max = obs_stat)
  } else if (direction %in% c("greater", "right")) {
    data.frame(x_min = obs_stat, x_max = Inf)
  }
}

two_tail_data <- function(obs_stat) {
  # Take advantage of {ggplot2} functionality to accept function as `data`
  # This is needed to make possible existence of `shade_p_value()` in case of
  # `direction = "both"`, as it depends on actual `data` but adding it as
  # argument to `shade_p_value()` is very bad.
  function(data) {
    if (get_viz_method(data) == "theoretical") {
      second_border <- -obs_stat
    } else {
      second_border <- mirror_obs_stat(data$stat, obs_stat)
    }
    
    data.frame(
      x_min = c(-Inf, max(obs_stat, second_border)),
      x_max = c(min(obs_stat, second_border), Inf)
    )
  }
}

get_viz_method <- function(data) {
  attr(data, "viz_method")
}
