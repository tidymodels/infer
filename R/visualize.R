#' @importFrom ggplot2 ggplot_add
#' @export
ggplot2::ggplot_add

#' Visualize statistical inference
#'
#' @description
#'
#' Visualize the distribution of the simulation-based inferential statistics or
#' the theoretical distribution (or both!).
#' 
#' Learn more in `vignette("infer")`.
#'
#' @param data A distribution. For simulation-based inference, a data frame 
#'   containing a distribution of [calculate()]d statistics 
#'   or [`fit()`][fit.infer()]ted coefficient estimates. This object should 
#'   have been passed to [generate()] before being supplied or 
#'   [calculate()] to [`fit()`][fit.infer()]. For theory-based inference,
#'   the output of [assume()].
#' @param bins The number of bins in the histogram.
#' @param method A string giving the method to display. Options are
#'   `"simulation"`, `"theoretical"`, or `"both"` with `"both"` corresponding to
#'   `"simulation"` and `"theoretical"`. If `data` is the output of [assume()],
#'   this argument will be ignored and default to `"theoretical"`.
#' @param dens_color A character or hex string specifying the color of the
#'   theoretical density curve.
#' @param ... Other arguments passed along to \\{ggplot2\\} functions.
#'
#' @details In order to make the visualization workflow more straightforward 
#' and explicit, `visualize()` now only should be used to plot distributions
#' of statistics directly. A number of arguments related to shading p-values and 
#' confidence intervals are now deprecated in `visualize()` and should
#' now be passed to [shade_p_value()] and [shade_confidence_interval()],
#' respectively. [visualize()] will raise a warning if deprecated arguments
#' are supplied.
#'
#' @return 
#' 
#' For [calculate()]-based workflows, a ggplot showing the simulation-based
#' distribution as a histogram or bar graph. Can also be used to display
#' theoretical distributions.
#'  
#' For [assume()]-based workflows, a ggplot showing the theoretical distribution.
#' 
#' For [`fit()`][fit.infer()]-based workflows, a `patchwork` object
#' showing the simulation-based distributions as a histogram or bar graph.
#' The interface to adjust plot options and themes is a bit different
#' for `patchwork` plots than ggplot2 plots. The examples highlight the
#' biggest differences here, but see [patchwork::plot_annotation()] and
#' [patchwork::&.gg] for more details.
#'
#' @seealso [shade_p_value()], [shade_confidence_interval()].
#'
#' @examples
#'   
#' # generate a null distribution
#' null_dist <- gss %>%
#'   # we're interested in the number of hours worked per week
#'   specify(response = hours) %>%
#'   # hypothesizing that the mean is 40
#'   hypothesize(null = "point", mu = 40) %>%
#'   # generating data points for a null distribution
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   # calculating a distribution of means
#'   calculate(stat = "mean")
#'   
#' # or a bootstrap distribution, omitting the hypothesize() step,
#' # for use in confidence intervals
#' boot_dist <- gss %>%
#'   specify(response = hours) %>%
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   calculate(stat = "mean")
#'   
#' # we can easily plot the null distribution by piping into visualize
#' null_dist %>%
#'   visualize()
#'
#' # we can add layers to the plot as in ggplot, as well... 
#' # find the point estimate---mean number of hours worked per week
#' point_estimate <- gss %>%
#'   specify(response = hours) %>%
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
#' # display a shading of the area beyond the p-value on the plot
#' null_dist %>%
#'   visualize() +
#'   shade_p_value(obs_stat = point_estimate, direction = "two-sided")
#' 
#' # ...or within the bounds of the confidence interval
#' null_dist %>%
#'   visualize() +
#'   shade_confidence_interval(ci)
#'   
#' # plot a theoretical sampling distribution by creating
#' # a theory-based distribution with `assume()`
#' sampling_dist <- gss %>%
#'   specify(response = hours) %>%
#'   assume(distribution = "t") 
#'   
#' visualize(sampling_dist)
#' 
#' # you can shade confidence intervals on top of
#' # theoretical distributions, too---the theoretical
#' # distribution will be recentered and rescaled to
#' # align with the confidence interval
#' visualize(sampling_dist) +
#'   shade_confidence_interval(ci)
#' 
#' 
#' # to plot both a theory-based and simulation-based null distribution,
#' # use a theorized statistic (i.e. one of t, z, F, or Chisq)
#' # and supply the simulation-based null distribution
#' null_dist_t <- gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40) %>%
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   calculate(stat = "t")
#'   
#' obs_stat <- gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40) %>%
#'   calculate(stat = "t")
#'
#' visualize(null_dist_t, method = "both")
#'
#' visualize(null_dist_t, method = "both") +
#'   shade_p_value(obs_stat, "both")
#' 
#' \donttest{
#' # to visualize distributions of coefficients for multiple
#' # explanatory variables, use a `fit()`-based workflow
#' 
#' # fit 1000 models with the `hours` variable permuted
#' null_fits <- gss %>%
#'  specify(hours ~ age + college) %>%
#'  hypothesize(null = "independence") %>%
#'  generate(reps = 1000, type = "permute") %>%
#'  fit()
#'  
#' null_fits
#' 
#' # visualize distributions of resulting coefficients
#' visualize(null_fits)
#' 
#' # the interface to add themes and other elements to patchwork
#' # plots (outputted by `visualize` when the inputted data
#' # is from the `fit()` function) is a bit different than adding
#' # them to ggplot2 plots.
#' library(ggplot2)
#' 
#' # to add a ggplot2 theme to a `calculate()`-based visualization, use `+`
#' null_dist %>% visualize() + theme_dark()
#'   
#' # to add a ggplot2 theme to a `fit()`-based visualization, use `&`
#' null_fits %>% visualize() & theme_dark()
#' }
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
                      ...) {
  if (inherits(data, "infer_dist")) {
    if (!missing(method) && method != "theoretical") {
      warning_glue(
        "Simulation-based visualization methods are not well-defined for ",
        "`assume()` output; the `method` argument will be ignored. Set ",
        '`method = "theoretical"` to silence this message.'
      )
    }
    
    method <- "theoretical"
    do_warn <- FALSE
  } else {
    if (method == "theoretical") {
      message_glue(
        'Rather than setting `method = "theoretical"` with a simulation-based ',
        'null distribution, the preferred method for visualizing theory-based ',
        'distributions with infer is now to pass the output of `assume()` as ',
        'the first argument to `visualize()`.'
      )
    }
    
    do_warn <- TRUE
  }
  
  attr(data, "viz_method") <- method
  attr(data, "viz_bins") <- bins
  
  dots <- check_dots_for_deprecated(list(...))
  
  if (is_fitted(data)) {
    term_data <- data %>%
      dplyr::rename(stat = estimate) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(term) %>%
      dplyr::group_split() %>%
      purrr::map(copy_attrs, data) %>%
      purrr::map(copy_attrs, data, c("viz_method", "viz_bins"))
    
    plots <- purrr::map2(
      term_data,
      purrr::map(term_data, purrr::pluck, "term", 1),
      visualize_term,
      bins = bins, 
      method = method, 
      dots = dots
    )
    
    return(
      patchwork::wrap_plots(plots, ncol = 1) +
        title_layer(
          term_data[[1]], 
          title_fn = patchwork::plot_annotation
        )
    )
  } else {
    res <- visualize_term(
      data = data,
      term = "stat",
      bins = bins, 
      method = method, 
      dens_color = dens_color,
      dots = dots,
      do_warn = do_warn
    ) + 
      title_layer(data)
    
    res
  }
}

#' @rdname visualize
#' @export
visualise <- visualize


visualize_term <- function(data, term, bins = 15, method = "simulation",
                           dens_color = "black", dots, do_warn = TRUE) {
  data <- check_for_nan(data, "visualize")
  check_visualize_args(data, bins, method, dens_color)
  plot_data <- create_plot_data(data)
  
  infer_plot <- ggplot(plot_data) +
    simulation_layer(data, dots = dots) +
    theoretical_layer(data, dens_color, dots = dots, do_warn = do_warn) +
    labels_layer(data, term)
  
  infer_plot
}

check_dots_for_deprecated <- function(dots) {
  dep_args <- c("obs_stat", "obs_stat_color", "pvalue_fill", "direction",
                "endpoints", "endpoints_color", "ci_fill")
  
  if (any(dep_args %in% names(dots))) {
    bad_args <- dep_args[dep_args %in% names(dots)]
    
    warning_glue(
      "The arguments `{list(bad_args)}` are deprecated in `visualize()` ",
      "and will be ignored. They should now be passed to one of ",
      "`shade_p_value()` or `shade_confidence_interval()`."
    )
    
    dots[!dep_args %in% names(dots)]
  }
  
  list(NULL)
}

check_visualize_args <- function(data, bins, method, dens_color) {
  check_is_distribution(data, "visualize")
  check_type(bins, is.numeric)
  check_type(method, is.character)
  check_type(dens_color, is.character)
  
  if (!(method %in% c("simulation", "theoretical", "both"))) {
    stop_glue(
      'Provide `method` with one of three options: `"theoretical"`, `"both"`, ',
      'or `"simulation"`. `"simulation"` is the default for simulation-based ',
      'null distributions, while `"theoretical"` is the only option for ',
      'null distributions outputted by `assume()`.'
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

impute_endpoints <- function(endpoints, plot = NULL) {
  res <- endpoints
  
  if (is_fitted(endpoints)) {
    x_lab <- x_axis_label(plot)
    
    res <- 
      endpoints %>% 
      dplyr::filter(term == x_lab) %>% 
      dplyr::select(-term)
    
    return(unlist(res))
  }
  
  if (is.vector(endpoints) && (length(endpoints) != 2)) {
    warning_glue(
      "Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector. ",
      "Using the first two entries as the `endpoints`."
    )
    res <- endpoints[1:2]
  }
  
  if (is.data.frame(endpoints)) {
    if ((nrow(endpoints) != 1) || (ncol(endpoints) != 2)) {
      stop_glue(
        "Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector."
      )
    }
    
    res <- unlist(endpoints)
  }
  
  res %>% copy_attrs(endpoints, attrs = c("se", "point_estimate"))
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

simulation_layer <- function(data, dots = list(NULL)) {
  method <- get_viz_method(data)
  bins <- get_viz_bins(data)
  
  if (method == "theoretical") {
    return(list())
  }
  
  # Manual computation of breaks is needed to fix histogram shape in future plot
  # buildings, e.g. after adding p-value areas.
  bin_breaks <- compute_bin_breaks(data, bins)
  
  if (method == "theoretical") {
    return(list())
  } 
  
  if (method == "simulation") {
    if (length(unique(data$stat)) >= 10) {
      res <- list(
        do.call(
          ggplot2::stat_bin,
          c(list(mapping = aes(x = stat), 
                 bins = bins, 
                 color = "white", 
                 breaks = bin_breaks), 
            dots)
        )
      )
    } else {
      # Probably should be removed
      res <- list(
        do.call(
          ggplot2::geom_bar,
          c(list(mapping = aes(x = stat)), dots)
        )
      )
    }
  } else if (method == "both") {
    res <- list(
      do.call(
        ggplot2::stat_bin,
        c(list(mapping = aes(x = stat, y = ggplot2::after_stat(density)),
               bins = bins,
               color = "white", 
               breaks = bin_breaks),
          dots)
        
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

theoretical_layer <- function(data, dens_color, dots = list(NULL), do_warn = TRUE,
                              mean_shift = 0, sd_shift = 1) {
  method <- get_viz_method(data)
  
  if (method == "simulation") {
    return(list())
  }
  
  warn_theoretical_layer(data, do_warn)
  
  theory_type <- short_theory_type(data)
  
  switch(
    theory_type,
    t = theory_curve(
      method, dt, qt, list(df = attr(data, "distr_param")), dens_color,
      mean_shift = mean_shift, sd_shift = sd_shift
    ),
    `F` = theory_curve(
      method, df, qf,
      list(
        df1 = attr(data, "distr_param"), df2 = attr(data, "distr_param2")
      ),
      dens_color = dens_color
    ),
    z = theory_curve(method, dnorm, qnorm, list(), dens_color, 
                     mean_shift = mean_shift, sd_shift = sd_shift),
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
    has_attr(data, "stat") &&
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

theory_curve <- function(method, d_fun, q_fun, args_list, dens_color,
                         mean_shift = 0, sd_shift = 1) {
  
  if (method == "theoretical") {
    d_fun_ <- shift_d_fun(d_fun, mean_shift, sd_shift)
    
    x_range <- (do.call(q_fun, c(p = list(c(0.001, 0.999)), args_list)) * 
                  sd_shift) +
      mean_shift
    
    res <- list(
      ggplot2::geom_path(
        data = data.frame(x = x_range), mapping = aes(x = x),
        stat = "function", fun = d_fun_, args = args_list,
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

shift_d_fun <- function(d_fun_, mean_shift, sd_shift) {
  function(x, ...) {
    d_fun_(x = (x - mean_shift) / sd_shift, ...)
  }
}

# when adding a confidence interval layer, rescale the theoretical
# layer 
redraw_theory_layer <- function(plot, mean_shift, sd_shift) {
  plot_data <- plot[["plot_env"]][["data"]]

  plot[["layers"]] <- 
    theoretical_layer(
      data = plot_data, 
      dens_color = plot[["plot_env"]][["dens_color"]], 
      dots = plot[["plot_env"]][["dots"]], 
      do_warn = plot[["plot_env"]][["do_warn"]],
      mean_shift = mean_shift, 
      sd_shift = sd_shift
    )
  
  plot
}


title_layer <- function(data, title_fn = ggplot2::ggtitle) {
  method <- get_viz_method(data)
  theory_type <- short_theory_type(data)
  
  if (is_hypothesized(data) || inherits(data, "infer_dist")) {
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
  
  if (is_fitted(data)) {
    plural <- "s"
  } else {
    plural <- ""
  }
  
  title_string <- switch(
    method,
    simulation = "Simulation-Based {distr_name}{plural}",
    theoretical = "Theoretical {theory_type} {distr_name}{plural}",
    both = "Simulation-Based and Theoretical {theory_type} {distr_name}s"
  )
  
  list(title_fn(glue_null(title_string)))
}

labels_layer <- function(data, term) {
  method <- get_viz_method(data)
  theory_type <- short_theory_type(data)
  
  x_lab <- switch(method, simulation = "{term}", "{theory_type} stat")
  y_lab <- switch(method, simulation = "count", "density")
  
  list(
    xlab(glue_null(x_lab)),
    ylab(glue_null(y_lab))
  )
}

facet_layer <- function() {
  list(
    ggplot2::facet_wrap(~term, scales = "free_x")
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
  
  if (!has_attr(x, "theory_type")) {
    return("")
  }
  
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

#' @method ggplot_add infer_layer
#' @export
ggplot_add.infer_layer <- function(object, plot, object_name) {
  # a method for the `+` operator for infer objects.
  # - "object to add" (arguments to the RHS of the `+`)
  # - plot is the existing plot (on the LHS of the `+`)
  # - object_name is the unevaluated call on the RHS of the `+`
  #
  # output is the actual output of the addition - this allows for
  # a more %>%-esque programming style
  #
  # the biggest advantage this offers us is that we can
  # overwrite existing elements, i.e. subsetting into the patchwork,
  # modifying its elements (for p-value and confidence interval shading),
  # and then overwriting them.
  #
  # both shade_p_value and shade_confidence_interval now just dispatch here
  # and execute term-wise along a patchwork object, so "object" is only a
  # stand-in classed object that sends to the right place
  
  # process object_name (shade_* call) ----------------------------------
  shade_fn <- attr(object, "fn")
  shade_args <- attributes(object)[!names(attributes(object)) %in% 
                                     c("class", "fn")]
  
  # if a patchwork object, use a custom `infer_layer` `+.gg` method.
  # otherwise, convert the `infer_layer` back to a list and call `+` again.
  if (inherits(plot, "patchwork")) {
    # use a for loop to invoke the `[[.patchwork` method
    n_patches <- length(plot$patches$plots) + 1
    
    new_plot <- plot
    
    for (i in 1:n_patches) {
      args <- shade_args
      args[["plot"]] <- plot[[i]]
      
      new_plot[[i]] <- 
        do.call(
          paste0(shade_fn, "_term"),
          args
        )
    }
  } else {
    args <- shade_args
    args[["plot"]] <- plot
    
    new_plot <- 
      do.call(
        paste0(shade_fn, "_term"),
        args
      )
  }
  
  new_plot
}

# extract the x axis label from a ggplot -- these are unique
# ids for terms in visualize() workflows
x_axis_label <- function(x) {
  x %>% purrr::pluck("labels", "x")
}

create_plot_data <- function(data) {
  if (inherits(data, "infer_dist")) {
    res <- tibble::tibble() %>%
      copy_attrs(data, 
                 c("theory_type", "distr_param", "distr_param2", "viz_method"))
  } else {
    res <- data
  }
  
  res
}
