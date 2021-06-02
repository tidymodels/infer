#' Compute confidence interval
#'
#' @description
#'
#' Compute a confidence interval around a summary statistic. Currently, only
#' simulation-based methods are supported.
#'
#' Learn more in `vignette("infer")`.
#'
#' @param x A data frame containing a distribution of [calculate()]d statistics 
#'   or [`fit()`][fit.infer()]ted coefficient estimates. This object should 
#'   have been passed to [generate()] before being supplied to [calculate()] 
#'   to [`fit()`][fit.infer()].
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
#' A null hypothesis is not required to compute a confidence interval, but
#' including `hypothesize()` in a chain leading to `get_confidence_interval()`
#' will not break anything. This can be useful when computing a confidence
#' interval after previously computing a p-value.
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
#' # For type = "se" or type = "bias-corrected" we need a point estimate
#' sample_mean <- gss %>%
#'   specify(response = hours) %>%
#'   calculate(stat = "mean") %>%
#'   dplyr::pull()
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
#' # using a model fitting workflow -----------------------
#' 
#' # fit a linear model predicting number of hours worked per
#' # week using respondent age and degree status.
#' observed_fit <- gss %>%
#'   specify(hours ~ age + college) %>%
#'   hypothesize(null = "independence") %>%
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
#' # More in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @name get_confidence_interval
#' @export
get_confidence_interval <- function(x, level = 0.95, type = "percentile",
                                    point_estimate = NULL) {
  # Inform if no `level` was explicitly supplied
  if (!("level" %in% rlang::call_args_names(match.call()))) {
    message_glue("Using `level = {level}` to compute confidence interval.")
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
      )
  } else {
    check_ci_args(x, level, type, point_estimate)
    
    switch_ci(type, x, level, point_estimate)
  }
}

#' @rdname get_confidence_interval
#' @export
get_ci <- function(x, level = 0.95, type = "percentile",
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
  ci_vec <- stats::quantile(x[[ncol(x)]], probs = (1 + c(-level, level)) / 2)

  make_ci_df(ci_vec)
}

ci_se <- function(x, level, point_estimate) {
  point_estimate <- check_obs_stat(point_estimate)

  multiplier <- stats::qnorm((1 + level) / 2)
  ci_vec <- point_estimate + c(-multiplier, multiplier) * stats::sd(x[[ncol(x)]])

  make_ci_df(ci_vec)
}

ci_bias_corrected <- function(x, level, point_estimate) {
  point_estimate <- check_obs_stat(point_estimate)

  p <- mean(x[[ncol(x)]] <= point_estimate)
  z0 <- stats::qnorm(p)
  # z_alpha_2 is z_(alpha/2)
  z_alpha_2 <- stats::qnorm((1 + c(-level, level)) / 2)
  new_probs <- stats::pnorm(2 * z0 + z_alpha_2)

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
  check_type(x, is.data.frame)
  check_type(level, is.numeric)

  if ((level <= 0) || (level >= 1)) {
    stop_glue("The value of `level` must be between 0 and 1 non-inclusive.")
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
}

make_ci_df <- function(ci_vec) {
  tibble::tibble(lower_ci = ci_vec[[1]], upper_ci = ci_vec[[2]])
}
