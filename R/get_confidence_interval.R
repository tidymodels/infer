#' Compute confidence interval
#'
#' @description
#'
#' Compute a confidence interval around a summary statistic. Both
#' simulation-based and theoretical methods are supported, though only
#' `type = "se"` is supported for theoretical methods.
#'
#' Learn more in `vignette("infer")`.
#'
#' @param x A distribution. For simulation-based inference, a data frame 
#'   containing a distribution of [calculate()]d statistics 
#'   or [`fit()`][fit.infer()]ted coefficient estimates. This object should 
#'   have been passed to [generate()] before being supplied or 
#'   [calculate()] to [`fit()`][fit.infer()]. For theory-based inference,
#'   output of [assume()]. Distributions for confidence intervals do not
#'   require a null hypothesis via [hypothesize()].
#' @param level A numerical value between 0 and 1 giving the confidence level.
#'   Default value is 0.95.
#' @param type A string giving which method should be used for creating the
#'   confidence interval. The default is `"percentile"` with `"se"`
#'   corresponding to (multiplier * standard error) and `"bias-corrected"` for
#'   bias-corrected interval as other options.
#' @param point_estimate A data frame containing the observed statistic (in a 
#'   [calculate()]-based workflow) or observed fit (in a 
#'   [`fit()`][fit.infer()]-based workflow). This object is likely the output 
#'   of [calculate()] or [`fit()`][fit.infer()] and need not
#'   to have been passed to [generate()]. Set to `NULL` by
#'   default. Must be provided if `type` is `"se"` or `"bias-corrected"`.
#'
#' @return A [tibble][tibble::tibble] containing the following columns:
#' 
#' \itemize{
#'   \item `term`: The explanatory variable (or intercept) in question. Only 
#'     supplied if the input had been previously passed to [`fit()`][fit.infer()].
#'   \item `lower_ci`, `upper_ci`: The lower and upper bounds of the confidence 
#'     interval, respectively.
#' }
#'
#' @details
#' A null hypothesis is not required to compute a confidence interval. However,
#' including [hypothesize()] in a pipeline leading to `get_confidence_interval()`
#' will not break anything. This can be useful when computing a confidence
#' interval using the same distribution used to compute a p-value.
#' 
#' Theoretical confidence intervals (i.e. calculated by supplying the output
#' of [assume()] to the `x` argument) require that the point estimate lies on
#' the scale of the data. The distribution defined in [assume()] will be
#' recentered and rescaled to align with the point estimate, as can be shown
#' in the output of [visualize()] when paired with [shade_confidence_interval()]. 
#' Confidence intervals are implemented for the following distributions and 
#' point estimates:
#'
#' \itemize{
#'   \item `distribution = "t"`: `point_estimate` should be the output of
#'   [calculate()] with `stat = "mean"` or `stat = "diff in means"`
#'   \item `distribution = "z"`: `point_estimate` should be the output of
#'   [calculate()] with `stat = "prop"` or `stat = "diff in props"`
#' } 
#'
#' @section Aliases:
#' `get_ci()` is an alias of `get_confidence_interval()`.
#' `conf_int()` is a deprecated alias of `get_confidence_interval()`.
#'
#' @examples
#'
#' boot_distr <- gss %>%
#'   # We're interested in the number of hours worked per week
#'   specify(response = hours) %>%
#'   # Generate bootstrap samples
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   # Calculate mean of each bootstrap sample
#'   calculate(stat = "mean")
#'
#' boot_distr %>%
#'   # Calculate the confidence interval around the point estimate
#'   get_confidence_interval(
#'     # At the 95% confidence level; percentile method
#'     level = 0.95
#'   )
#'
#' # for type = "se" or type = "bias-corrected" we need a point estimate
#' sample_mean <- gss %>%
#'   specify(response = hours) %>%
#'   calculate(stat = "mean")
#'
#' boot_distr %>%
#'   get_confidence_interval(
#'     point_estimate = sample_mean,
#'     # At the 95% confidence level
#'     level = 0.95,
#'     # Using the standard error method
#'     type = "se"
#'   )
#'   
#' # using a theoretical distribution -----------------------------------
#' 
#' # define a sampling distribution
#' sampling_dist <- gss %>%
#'   specify(response = hours) %>%
#'   assume("t")
#' 
#' # get the confidence interval---note that the
#' # point estimate is required here
#' get_confidence_interval(
#'   sampling_dist, 
#'   level = .95, 
#'   point_estimate = sample_mean
#' )
#'   
#' # using a model fitting workflow -----------------------
#' 
#' # fit a linear model predicting number of hours worked per
#' # week using respondent age and degree status.
#' observed_fit <- gss %>%
#'   specify(hours ~ age + college) %>%
#'   fit()
#' 
#' observed_fit
#' 
#' # fit 100 models to resamples of the gss dataset, where the response 
#' # `hours` is permuted in each. note that this code is the same as 
#' # the above except for the addition of the `generate` step.
#' null_fits <- gss %>%
#'   specify(hours ~ age + college) %>%
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 100, type = "permute") %>%
#'   fit()
#' 
#' null_fits
#' 
#' get_confidence_interval(
#'   null_fits, 
#'   point_estimate = observed_fit, 
#'   level = .95
#' )
#'
#' # more in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @name get_confidence_interval
#' @family auxillary functions
#' @export
get_confidence_interval <- function(x, level = 0.95, type = NULL,
                                    point_estimate = NULL) {
  # Inform if no `level` was explicitly supplied
  if (!("level" %in% rlang::call_args_names(match.call()))) {
    message_glue("Using `level = {level}` to compute confidence interval.")
  }
  
  if (is.null(type)) {
    if (inherits(x, "infer_dist")) {
      type <- "se"
    } else {
      type <- "percentile"
    }
  }
  
  if (is_fitted(x)) {
    # check that x and point estimate reference the same variables
    check_mlr_x_and_obs_stat(
      x, 
      point_estimate, 
      "get_confidence_interval", 
      "point_estimate"
    )
    
    # split up x and point estimate by term
    term_data <- x %>%
      dplyr::ungroup() %>%
      dplyr::group_by(term) %>%
      dplyr::group_split() %>%
      purrr::map(copy_attrs, x)
    
    term_estimates <- point_estimate %>%
      dplyr::ungroup() %>%
      dplyr::group_by(term) %>%
      dplyr::group_split()
    
    # check arguments for each term
    purrr::map2_dfr(
      term_data,
      purrr::map(term_estimates, purrr::pluck, "estimate"),
      check_ci_args,
      level = level,
      type = type
    )
    
    # map over switch_ci and then add the term column back in
    purrr::map2_dfr(
      term_data,
      purrr::map(term_estimates, purrr::pluck, "estimate"),
      switch_ci,
      level = level,
      type = type
    ) %>%
      dplyr::mutate(
        term = purrr::map_chr(term_estimates, purrr::pluck, "term"),
        .before = dplyr::everything()
      ) %>%
      copy_attrs(x)
  } else {
    check_ci_args(x, level, type, point_estimate)
    
    switch_ci(type, x, level, point_estimate)
  }
}

#' @rdname get_confidence_interval
#' @export
get_ci <- function(x, level = 0.95, type = NULL,
                   point_estimate = NULL) {
  get_confidence_interval(
    x,
    level = level, type = type, point_estimate = point_estimate
  )
}

switch_ci <- function(type, x, level, point_estimate) {
  switch(
    type,
    percentile = ci_percentile(x, level),
    se = ci_se(x, level, point_estimate),
    `bias-corrected` = ci_bias_corrected(x, level, point_estimate)
  )
}

ci_percentile <- function(x, level) {
  # x[[ncol(x)]] pulls out the stat or estimate column
  ci_vec <- stats::quantile(x[[ncol(x)]], probs = (1 + c(-level, level)) / 2)

  make_ci_df(ci_vec)
}

ci_se <- function(x, level, point_estimate) {
  point_estimate_ <- check_obs_stat(point_estimate)
  
  args <- list() 
  
  if (inherits(x, "infer_dist")) {
    se <- attr(point_estimate, "se")
    qfn <- paste0("q", attr(x, "distribution"))
    if (attr(x, "distribution") == "t") {
      args <- list(df = attr(x, "df"))
    }
  } else {
    # x[[ncol(x)]] pulls out the stat or estimate column
    se <- stats::sd(x[[ncol(x)]])
    qfn <- "qnorm"
  }

  args <- c(args, list(p = (1 + level) / 2))
  
  multiplier <- do.call(qfn, args)

  ci_vec <- point_estimate_ + c(-multiplier, multiplier) * se

  res <- make_ci_df(ci_vec)
  
  attr(res, "se") <- attr(point_estimate, "se")
  attr(res, "point_estimate") <- point_estimate_
  
  res
}

ci_bias_corrected <- function(x, level, point_estimate) {
  point_estimate <- check_obs_stat(point_estimate)

  # x[[ncol(x)]] pulls out the stat or estimate column
  p <- mean(x[[ncol(x)]] <= point_estimate)
  z0 <- stats::qnorm(p)
  # z_alpha_2 is z_(alpha/2)
  z_alpha_2 <- stats::qnorm((1 + c(-level, level)) / 2)
  new_probs <- stats::pnorm(2 * z0 + z_alpha_2)

  # x[[ncol(x)]] pulls out the stat or estimate column
  ci_vec <- stats::quantile(x[[ncol(x)]], probs = new_probs)

  make_ci_df(ci_vec)
}

check_ci_args <- function(x, level, type, point_estimate) {
  if (!is.null(point_estimate)) {
    if (!is.data.frame(point_estimate)) {
      check_type(point_estimate, is.numeric)
    } else {
      check_type(point_estimate, is.data.frame)
      check_type(point_estimate[[1]][[1]], is.numeric)
    }
  }
  check_is_distribution(x, "get_confidence_interval")
  check_type(level, is.numeric)

  if ((level <= 0) || (level >= 1)) {
    stop_glue("The value of `level` must be between 0 and 1 non-inclusive.")
  }
  
  if (inherits(x, "infer_dist") && !is.null(type) && type != "se") {
    stop_glue(
      'The only `type` option for theory-based confidence intervals ',
      'is `type = "se"`.'
    )
  }

  if (!(type %in% c("percentile", "se", "bias-corrected"))) {
    stop_glue(
      'The options for `type` are "percentile", "se", or "bias-corrected".'
    )
  }

  if ((type %in% c("se", "bias-corrected")) && is.null(point_estimate)) {
    stop_glue(
      "A numeric value needs to be given for `point_estimate` ",
      'for `type` "se" or "bias-corrected".'
    )
  }
  
  if (inherits(x, "infer_dist")) {
    # theoretical CIs require the full point estimate infer object as they
    # contain the necessary standard error
    if (!inherits(point_estimate, "infer")) {
      stop_glue(
        'For theoretical confidence intervals, the `point_estimate` argument ',
        'must be an `infer` object. Have you made sure to supply the output of ',
        '`calculate()` as the `point_estimate` argument?'
      )
    }
    
    if (!attr(point_estimate, "stat") %in% 
        c("mean", "prop", "diff in means", "diff in props")) {
      stop_glue(
        'The only allowable statistics for theoretical confidence intervals ',
        'are "mean", "prop", "diff in means", and "diff in props". See ',
        'the "Details" section of `?get_confidence_interval` for more details.'
      )
    }
    
    if ((attr(x, "distribution") == "t" && 
         !attr(point_estimate, "stat") %in% c("mean", "diff in means")) ||
        (attr(x, "distribution") == "norm" &&
         !attr(point_estimate, "stat") %in% c("prop", "diff in props"))) {
      stop_glue(
        'Confidence intervals using a `{attr(x, "dist_")}` distribution for ',
        '`stat = {attr(point_estimate, "stat")}` are not implemented.'
      )
    }
  }
}

make_ci_df <- function(ci_vec) {
  tibble::tibble(lower_ci = ci_vec[[1]], upper_ci = ci_vec[[2]])
}
