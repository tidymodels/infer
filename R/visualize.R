#' Visualize statistical inference
#'
#' @description
#'
#' Visualize the distribution of the simulation-based inferential statistics or
#' the theoretical distribution (or both!).
#' 
#' Learn more in `vignette("infer")`.
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
#'   
#' # find a null distribution
#' null_dist <- gss %>%
#'   # we're interested in the number of hours worked per week
#'   specify(response = hours) %>%
#'   # hypothesizing that the mean is 40
#'   hypothesize(null = "point", mu = 40) %>%
#'   # generating data points for a null distribution
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   # calculating a distribution of t test statistics
#'   calculate(stat = "t")
#'   
#' # we can easily plot the null distribution by piping into visualize
#' null_dist %>%
#'   visualize()
#'
#' # we can add layers to the plot as in ggplot, as well... 
#' # find the point estimate---mean number of hours worked per week
#' point_estimate <- gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40) %>%
#'   calculate(stat = "t")
#'   
#' # find a confidence interval around the point estimate
#' ci <- null_dist %>%
#'   get_confidence_interval(point_estimate = point_estimate,
#'                           # at the 95% confidence level
#'                           level = .95,
#'                           # using the standard error method
#'                           type = "se")  
#'   
#' # display a shading of the area beyond the p-value on the plot
#' null_dist %>%
#'   visualize() +
#'   shade_p_value(obs_stat = point_estimate, direction = "two-sided")
#' 
#' null_dist %>%
#'   visualize() +
#'   shade_confidence_interval(ci)
#'   
#' # to plot a theoretical null distribution, skip the generate()
#' # step and supply `method = "theoretical"` to `visualize()`
#' null_dist_theoretical <- gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40) %>%
#'   calculate(stat = "t") 
#'   
#' visualize(null_dist_theoretical, method = "theoretical")
#' 
#' # to plot both a theory-based and simulation-based null distribution,
#' # use the simulation-based null distribution and supply
#' # `method = "both"` to `visualize()`
#' visualize(null_dist, method = "both")
#'
#' # More in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @importFrom ggplot2 ggplot geom_histogram aes ggtitle
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
  data <- check_for_nan(data, "visualize")
  check_visualize_args(
    data, bins, method, dens_color, obs_stat, obs_stat_color,
    pvalue_fill, direction, endpoints, endpoints_color, ci_fill
  )
  warn_deprecated_args(obs_stat, endpoints)
  endpoints <- impute_endpoints(endpoints)
  obs_stat <- impute_obs_stat(obs_stat, direction, endpoints)

  # Add `method` to `data` attributes to enable later possibility of
  # complicated computation of p-value regions (in case `direction = "both"`)
  # in `shade_p_value()`.
  attr(data, "viz_method") <- method
  attr(data, "viz_bins") <- bins

  infer_plot <- ggplot(data) +
    simulation_layer(data, ...) +
    theoretical_layer(data, dens_color, ...) +
    title_labels_layer(data) +
    shade_p_value(
      obs_stat, direction, obs_stat_color, pvalue_fill, ...
    )

  if (!is.null(direction) && (direction == "between")) {
    infer_plot <- infer_plot +
      shade_confidence_interval(endpoints, endpoints_color, ci_fill, ...)
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

  TRUE
}

# a function for checking arguments to functions that are added as layers
# to visualize()d objects to make sure they weren't mistakenly piped
check_for_piped_visualize <- function(...) {
  
  is_ggplot_output <- vapply(list(...), ggplot2::is.ggplot, logical(1))
  
  if (any(is_ggplot_output)) {
    
    called_function <- sys.call(-1)[[1]]
    
    stop_glue(
      "It looks like you piped the result of `visualize()` into ",
      "`{called_function}()` (using `%>%`) rather than adding the result of ",
      "`{called_function}()` as a layer with `+`. Consider changing",
      "`%>%` to `+`."
    )
  }
  
  TRUE
}

warn_deprecated_args <- function(obs_stat, endpoints) {
  if (!is.null(obs_stat)) {
    warning_glue(
      "`visualize()` should no longer be used to plot a p-value. Arguments `obs_stat`, ",
      "`obs_stat_color`, `pvalue_fill`, and `direction` are deprecated. ",
      "Use `shade_p_value()` instead."
    )
  }

  if (!is.null(endpoints)) {
    warning_glue(
      "`visualize()` should no longer be used to plot a confidence interval. Arguments ",
      "`endpoints`, `endpoints_color`, and `ci_fill` are deprecated. ",
      "Use `shade_confidence_interval()` instead."
    )
  }

  TRUE
}

impute_endpoints <- function(endpoints) {
  if (is.vector(endpoints) && (length(endpoints) != 2)) {
    warning_glue(
      "Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector. ",
      "Using the first two entries as the `endpoints`."
    )
    endpoints <- endpoints[1:2]
  }

  if (is.data.frame(endpoints)) {
    if ((nrow(endpoints) != 1) || (ncol(endpoints) != 2)) {
      stop_glue(
        "Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector."
      )
    }

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

simulation_layer <- function(data, ...) {
  method <- get_viz_method(data)
  bins <- get_viz_bins(data)

  if (method == "theoretical") {
    return(list())
  }
  
  # Manual computation of breaks is needed to fix histogram shape in future plot
  # buildings, e.g. after adding p-value areas.
  bin_breaks <- compute_bin_breaks(data, bins)

  if (method == "simulation") {
    if (length(unique(data$stat)) >= 10) {
      res <- list(
        ggplot2::stat_bin(
          mapping = aes(x = stat), bins = bins, color = "white", ...,
          breaks = bin_breaks
        )
      )
    } else {
      # Probably should be removed
      res <- list(geom_bar(mapping = aes(x = stat), ...))
    }
  } else if (method == "both") {
    res <- list(
      ggplot2::stat_bin(
        mapping = aes(x = stat, y = ..density..), bins = bins,
        color = "white", ..., breaks = bin_breaks
      )
    )
  }

  res
}

compute_bin_breaks <- function(data, bins) {
  g <- ggplot(data) + ggplot2::stat_bin(aes(stat), bins = bins)
  g_tbl <- ggplot2::ggplot_build(g)[["data"]][[1]]
  
  c(g_tbl[["xmin"]][1], g_tbl[["xmax"]])
}

theoretical_layer <- function(data, dens_color, ..., do_warn = TRUE) {
  method <- get_viz_method(data)

  if (method == "simulation") {
    return(list())
  }

  warn_theoretical_layer(data, do_warn)

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

warn_theoretical_layer <- function(data, do_warn = TRUE) {
  if (!do_warn) {
    return(TRUE)
  }
  
  method <- get_viz_method(data)

  warning_glue(
    "Check to make sure the conditions have been met for the theoretical ",
    "method. {{infer}} currently does not check these for you."
  )

  if (
    !has_attr(data, "stat") &&
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
      ggplot2::geom_path(
        data = data.frame(x = x_range), mapping = aes(x = x),
        stat = "function", fun = d_fun, args = args_list,
        color = dens_color
      )
    )
  } else if (method == "both") {
    res <- list(
      ggplot2::geom_path(
        mapping = aes(x = stat),
        stat = "function", fun = d_fun, args = args_list,
        color = dens_color
      )
    )
  }

  res
}

title_labels_layer <- function(data) {
  method <- get_viz_method(data)
  theory_type <- short_theory_type(data)
  
  if (is_hypothesized(data)) {
    distr_name <- "Null Distribution"
  } else {
    distr_name <- switch(
      attr(data, "type"),
      bootstrap = "Bootstrap Distribution",
      # For other generation types there will be no distribution adjective.
      # However, currently they seem to be never used without `hypothesize()`
      # step.
      "Distribution"
    )
  }

  title_string <- switch(
    method,
    simulation = "Simulation-Based {distr_name}",
    theoretical = "Theoretical {theory_type} {distr_name}",
    both = "Simulation-Based and Theoretical {theory_type} {distr_name}s"
  )

  x_lab <- switch(method, simulation = "stat", "{theory_type} stat")
  y_lab <- switch(method, simulation = "count", "density")

  list(
    ggtitle(glue_null(title_string)),
    xlab(glue_null(x_lab)),
    ylab(glue_null(y_lab))
  )
}

check_shade_confidence_interval_args <- function(color, fill) {
  check_type(color, is_color_string, "color string")
  if (!is.null(fill)) {
    check_type(fill, is_color_string, "color string")
  }
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

get_viz_method <- function(data) {
  attr(data, "viz_method")
}

get_viz_bins <- function(data) {
  attr(data, "viz_bins")
}
