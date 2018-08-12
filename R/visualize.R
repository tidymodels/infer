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
#'   observed statistic is.
#' @param obs_stat_color A character or hex string specifying the color of the
#'   observed statistic as a vertical line on the plot.
#' @param pvalue_fill A character or hex string specifying the color to shade
#'   the p-value. In previous versions of the package this was the `shade_color`
#'   argument.
#' @param direction A string specifying in which direction the shading should
#'   occur. Options are `"less"`, `"greater"`, or `"two_sided"` for p-value. Can
#'   also give `"left"`, `"right"`, or `"both"` for p-value. For confidence
#'   intervals, use `"between"` and give the endpoint values in `endpoints`.
#' @param endpoints A 2 element vector or a 1 x 2 data frame containing the
#'   lower and upper values to be plotted. Most useful for visualizing
#'   conference intervals.
#' @param endpoints_color A character or hex string specifying the color of the
#'   observed statistic as a vertical line on the plot.
#' @param ci_fill A character or hex string specifying the color to shade the
#'   confidence interval.
#' @param ... Other arguments passed along to ggplot2.
#'
#' @return A ggplot object showing the simulation-based distribution as a
#'   histogram or bar graph. Also used to show the theoretical curves.
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

  if (method == "simulation") {
    infer_plot <- visualize_simulation(
      data = data,
      bins = bins,
      dens_color = dens_color,
      obs_stat = obs_stat,
      obs_stat_color = obs_stat_color,
      direction = direction,
      pvalue_fill = pvalue_fill,
      endpoints = endpoints,
      ci_fill = ci_fill,
      ...
    )
  } else if (method == "theoretical") {
    infer_plot <- visualize_theoretical(
      data = data,
      dens_color = dens_color,
      obs_stat = obs_stat,
      obs_stat_color = obs_stat_color,
      direction = direction,
      pvalue_fill = pvalue_fill,
      endpoints = endpoints,
      ci_fill = ci_fill,
      ...
    )
  } else if (method == "both") {
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
        "With only {length(unique(data$stat))} replicates, it may be ",
        "difficult to see the relationship between simulation and theory."
      )
    }

    infer_plot <- visualize_both(
      data = data,
      bins = bins,
      dens_color = dens_color,
      obs_stat = obs_stat,
      obs_stat_color = obs_stat_color,
      direction = direction,
      pvalue_fill = pvalue_fill,
      endpoints = endpoints,
      ci_fill = ci_fill,
      ...
    )
  } else {
    stop_glue(
      'Provide `method` with one of three options: `"theoretical"`, `"both"`, ',
      'or `"simulation"`. `"simulation"` is the default.'
    )
  }

  if (!is.null(obs_stat)) { # && !is.null(direction)
    infer_plot <- infer_plot +
      geom_vline(xintercept = obs_stat, size = 2, color = obs_stat_color, ...)
  }

  if (!is.null(endpoints)) {
    if (!is.null(obs_stat)) {
      warning_glue(
        "Values for both `endpoints` and `obs_stat` were given when only one ",
        "should be set. Ignoring `obs_stat` values."
      )
    }
    infer_plot <- infer_plot +
      geom_vline(
        xintercept = endpoints, size = 2, color = endpoints_color, ...
      )
  }

  infer_plot
}

theory_plot <- function(d_fun, q_fun, args_list, stat_name, dens_color) {
  x_range <- do.call(q_fun, c(p = list(c(0.001, 0.999)), args_list))
  
  ggplot(data.frame(x = x_range)) +
    stat_function(
      mapping = aes(x), fun = d_fun, args = args_list, color = dens_color
    ) +
    ggtitle(glue_null("Theoretical {stat_name} Null Distribution")) +
    xlab("") + ylab("")
}

both_plot <- function(data, d_fun, args_list, stat_name, stat_label, dens_color,
                      obs_stat, direction, bins, pvalue_fill, endpoints,
                      ci_fill, ...) {
  infer_plot <- shade_density_check(
    data = data,
    obs_stat = obs_stat,
    direction = direction,
    bins = bins,
    endpoints = endpoints,
    pvalue_fill = pvalue_fill,
    ci_fill = ci_fill
  )
  
  infer_plot +
    stat_function(
      fun = d_fun, args = args_list, color = dens_color
    ) +
    ggtitle(glue_null(
      "Simulation-Based and Theoretical {stat_name} Null Distributions"
    )) +
    xlab(stat_label) + ylab("")
}

shade_density_check <- function(data,
                                obs_stat,
                                direction,
                                bins,
                                density = TRUE,
                                pvalue_fill,
                                endpoints,
                                ci_fill,
                                ...) {
  if (is.null(direction) || is.null(obs_stat)) {
    if (density) {
      gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
        geom_histogram(
          bins = bins, color = "white", mapping = aes(y = ..density..), ...
        )
    } # else {
      # Not sure if needed? Can't get tests to find it
      # gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
      #  geom_histogram(bins = bins, color = "white", ...)
    # }
  }

  if (xor(!is.null(obs_stat), !is.null(endpoints))) {
    if (!is.null(direction)) {
      if (density) {
        gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
          geom_histogram(
            bins = bins, color = "white", mapping = aes(y = ..density..), ...
          )
      } else {
        gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
          geom_histogram(bins = bins, color = "white", ...)
      }

      if (direction %in% c("less", "left", "greater", "right")) {
        gg_plot <- gg_plot +
          geom_tail(direction, obs_stat, pvalue_fill)
      }
      
      if (direction %in% c("two_sided", "both")) {
        gg_plot <- gg_plot +
          geom_both_tails(
            border_1 = obs_stat,
            border_2 = mirror_obs_stat(data$stat, obs_stat),
            fill = pvalue_fill
          )
      }

      if (direction == "between") {
        gg_plot <- gg_plot +
          geom_rect(
            data = data.frame(endpoints[1]),
            fill = ci_fill, alpha = 0.6,
            aes(xmin = endpoints[1], xmax = endpoints[2], ymin = 0, ymax = Inf),
            inherit.aes = FALSE,
            ...
          )
      }
    }
  }
  gg_plot
}

visualize_simulation <- function(data, bins,
                                 method = "simulation",
                                 dens_color,
                                 obs_stat,
                                 obs_stat_color,
                                 direction,
                                 pvalue_fill,
                                 endpoints,
                                 ci_fill,
                                 ...) {
  if (is.null(direction)) {
    if (length(unique(data$stat)) >= 10) {
      infer_plot <- ggplot(data = data, mapping = aes(x = stat)) +
        geom_histogram(bins = bins, color = "white", ...)
    } else {
      infer_plot <- ggplot(data = data, mapping = aes(x = stat)) +
        geom_bar(...) +
        xlab("stat")
    }
  } else {
    infer_plot <- shade_density_check(
      data = data,
      obs_stat = obs_stat,
      direction = direction,
      bins = bins,
      density = FALSE,
      pvalue_fill = pvalue_fill,
      endpoints = endpoints,
      ci_fill = ci_fill
    )
  }
  infer_plot
}

visualize_theoretical <- function(data,
                                  dens_color,
                                  obs_stat,
                                  obs_stat_color,
                                  direction,
                                  pvalue_fill,
                                  endpoints,
                                  ci_fill,
                                  ...) {
  warning_glue(
    "Check to make sure the conditions have been met for the theoretical ",
    "method. {{infer}} currently does not check these for you."
  )

  if (
    !is_nuat(data, "stat") &&
    !(attr(data, "stat") %in% c("t", "z", "Chisq", "F"))
  ) {
    warning_glue(
      "Your `calculate`d statistic and the theoretical distribution are on ",
      "different scales. Displaying only the theoretical distribution."
    )
  }

  if (
    attr(data, "theory_type") %in% c(
      "Two sample t", "Slope with t", "One sample t"
    )
  ) {
    infer_plot <- theory_plot(
      d_fun = dt, q_fun = qt,
      args_list = list(df = attr(data, "distr_param")),
      stat_name = "t",
      dens_color = dens_color
    )
  } else if (attr(data, "theory_type") == "ANOVA") {
    warn_right_tail_test(direction, "F")

    infer_plot <- theory_plot(
      d_fun = df, q_fun = qf,
      args_list = list(
        df1 = attr(data, "distr_param"), df2 = attr(data, "distr_param2")
      ),
      stat_name = "F",
      dens_color = dens_color
    )
  } else if (
    attr(data, "theory_type") %in% c("One sample prop z", "Two sample props z")
  ) {
    infer_plot <- theory_plot(
      d_fun = dnorm, q_fun = qnorm,
      args_list = list(),
      stat_name = "z",
      dens_color = dens_color
    )
  } else if (
    attr(data, "theory_type") %in% c(
      "Chi-square test of indep", "Chi-square Goodness of Fit"
    )
  ) {
    warn_right_tail_test(direction, "Chi-square")

    infer_plot <- theory_plot(
      d_fun = dchisq, q_fun = qchisq,
      args_list = list(df = attr(data, "distr_param")),
      stat_name = "Chi-Square",
      dens_color = dens_color
    )
  } # else {
    # stop_glue(
    #   '"{attr(data, "theory_type")}" is not implemented (possibly yet).'
    # )
  # }

  # Plot tails
  if (!is.null(obs_stat) && !is.null(direction)) {
    if (direction %in% c("less", "left", "greater", "right")) {
      infer_plot <- infer_plot +
        geom_tail(direction, obs_stat, pvalue_fill, ...)
    }
    # Assuming two-tailed shading will only happen with theoretical
    # distributions centered at 0
    if (direction %in% c("two_sided", "both")) {
      infer_plot <- infer_plot +
        geom_both_tails(obs_stat, -obs_stat, pvalue_fill, ...)
    }
  }

  # To implement: plotting of theoretical confidence interval values

  infer_plot
}

visualize_both <- function(data, bins,
                           dens_color,
                           obs_stat,
                           obs_stat_color,
                           direction,
                           pvalue_fill,
                           endpoints,
                           ci_fill,
                           ...) {
  warning_glue(
    "Check to make sure the conditions have been met for the theoretical ",
    "method. `infer` currently does not check these for you."
  )

  if (!(attr(data, "stat") %in% c("t", "z", "Chisq", "F"))) {
    stop_glue(
      "Your `calculate`d statistic and the theoretical distribution are on ",
      "different scales. Use a standardized `stat` instead."
    )
  }

  if (attr(data, "theory_type") %in% c("Two sample t", "Slope with t")) {
    infer_plot <- both_plot(
      data = data,
      d_fun = dt,
      args_list = list(df = attr(data, "distr_param")),
      stat_name = "t", stat_label = "tstat",
      dens_color = dens_color,
      bins = bins,
      direction = direction,
      obs_stat = obs_stat,
      pvalue_fill = pvalue_fill,
      endpoints = endpoints,
      ci_fill = ci_fill
    )
  } else if (attr(data, "theory_type") == "ANOVA") {
    warn_right_tail_test(direction, "F")
    
    infer_plot <- both_plot(
      data = data,
      d_fun = df,
      args_list = list(
        df1 = attr(data, "distr_param"), df2 = attr(data, "distr_param2")
      ),
      stat_name = "F", stat_label = "Fstat",
      dens_color = dens_color,
      bins = bins,
      direction = direction,
      obs_stat = obs_stat,
      pvalue_fill = pvalue_fill,
      endpoints = endpoints,
      ci_fill = ci_fill
    )
  } else if (
    attr(data, "theory_type") %in% c("One sample prop z", "Two sample props z")
  ) {
    infer_plot <- both_plot(
      data = data,
      d_fun = dnorm,
      args_list = list(),
      stat_name = "z", stat_label = "zstat",
      dens_color = dens_color,
      bins = bins,
      direction = direction,
      obs_stat = obs_stat,
      pvalue_fill = pvalue_fill,
      endpoints = endpoints,
      ci_fill = ci_fill
    )
  } else if (
    attr(data, "theory_type") %in% c(
      "Chi-square test of indep", "Chi-square Goodness of Fit"
    )
  ) {
    warn_right_tail_test(direction, "Chi-square")
    
    infer_plot <- both_plot(
      data = data,
      d_fun = dchisq,
      args_list = list(df = attr(data, "distr_param")),
      stat_name = "Chi-Square", stat_label = "chisqstat",
      dens_color = dens_color,
      bins = bins,
      direction = direction,
      obs_stat = obs_stat,
      pvalue_fill = pvalue_fill,
      endpoints = endpoints,
      ci_fill = ci_fill
    )
  } # else {
    # stop_glue('"{attr(data, "theory_type")}" is not implemented yet.')
  # }

  infer_plot
}

get_percentile <- function(vector, observation) {
  stats::ecdf(vector)(observation)
}

mirror_obs_stat <- function(vector, observation) {
  obs_percentile <- get_percentile(vector, observation)
  
  stats::quantile(vector, probs = 1 - obs_percentile)
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

geom_tail <- function(dir, border, fill, ...) {
  if (dir %in% c("less", "left")) {
    x_range <- c(-Inf, border)
  } else if (dir %in% c("greater", "right")) {
    x_range <- c(border, Inf)
  }
  
  list(
    geom_rect(
      data = data.frame(border),
      aes(xmin = x_range[1], xmax = x_range[2], ymin = 0, ymax = Inf),
      fill = fill, alpha = 0.6,
      inherit.aes = FALSE,
      ...
    )
  )
}

geom_both_tails <- function(border_1, border_2, fill, ...) {
  left_border <- min(border_1, border_2)
  right_border <- max(border_1, border_2)
  
  c(
    geom_tail("left", left_border, fill, ...),
    geom_tail("right", right_border, fill, ...)
  )
}
