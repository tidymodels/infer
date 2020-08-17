#' Compute confidence interval
#'
#' @description
#'
#' Compute a confidence interval around a summary statistic. Currently only
#' simulation-based methods are supported.
#'
#' Learn more in `vignette("infer")`.
#'
#' @param x Data frame of calculated statistics or containing attributes of
#'   theoretical distribution values. Currently, dependent on statistics being
#'   stored in `stat` column as created in [calculate()] function.
#' @param level A numerical value between 0 and 1 giving the confidence level.
#'   Default value is 0.95.
#' @param type A string giving which method should be used for creating the
#'   confidence interval. The default is `"percentile"` with `"se"`
#'   corresponding to (multiplier * standard error) and `"bias-corrected"` for
#'   bias-corrected interval as other options.
#' @param point_estimate A numeric value or a 1x1 data frame set to `NULL` by
#'   default. Needed to be provided if `type` is `"se"` or `"bias-corrected"`.
#'
#' @return A 1 x 2 tibble with 'lower_ci' and 'upper_ci' columns. Values
#'   correspond to lower and upper bounds of the confidence interval.
#`
#' @details
#'   A null hypothesis is not required to compute a confidence interval,
#'   but including `hypothesize()` in a chain leading to `get_confidence_interval()`
#'   will not break anything.  This can be useful when computing a confidence
#'   interval after previously computing a p-value.
#'
#'
#' @section Aliases:
#'   `get_ci()` is an alias of `get_confidence_interval()`.
#'   `conf_int()` is a deprecated alias of `get_confidence_interval()`.
#'
#' @examples
#'
#' gss %>%
#'   # we're interested in the number of hours worked per week
#'   specify(response = hours) %>%
#'   # generate bootstrap samples
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   # calculate mean of each bootstrap sample
#'   calculate(stat = "mean") %>%
#'   # calculate the confidence interval around the point estimate
#'   get_confidence_interval(
#'     # at the 95% confidence level; percentile method
#'     level = 0.95
#'   )
#'
#' # for type = "se" or type = "bias-corrected" we need a point estimate
#' sample_mean <- gss %>%
#'   specify(response = hours) %>%
#'   calculate(stat = "mean") %>%
#'   dplyr::pull()
#'
#' gss %>%
#'   # ...we're interested in the number of hours worked per week
#'   specify(response = hours) %>%
#'   # generating data points for a null distribution
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   # finding the null distribution
#'   calculate(stat = "mean") %>%
#    # calculate the confidence interval around the point estimate
#'   get_confidence_interval(
#'     point_estimate = sample_mean,
#'     # at the 95% confidence level
#'     level = 0.95,
#'     # using the standard error method
#'     type = "se"
#'   )
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
  check_ci_args(x, level, type, point_estimate)

  # Inform if no `level` was explicitly supplied
  if (!("level" %in% rlang::call_args_names(match.call()))) {
    message_glue("Using `level = {level}` to compute confidence interval.")
  }

  switch(
    type,
    percentile = ci_percentile(x, level),
    se = ci_se(x, level, point_estimate),
    `bias-corrected` = ci_bias_corrected(x, level, point_estimate)
  )
}

#' @rdname get_confidence_interval
#' @export
get_ci <- function(x, level = 0.95, type = "percentile",
                   point_estimate = NULL) {
  get_confidence_interval(
    x, level = level, type = type, point_estimate = point_estimate
  )
}

ci_percentile <- function(x, level) {
  ci_vec <- stats::quantile(x[["stat"]], probs = (1 + c(-level, level)) / 2)

  make_ci_df(ci_vec)
}

ci_se <- function(x, level, point_estimate) {
  point_estimate <- check_obs_stat(point_estimate)

  multiplier <- stats::qnorm((1 + level) / 2)
  ci_vec <- point_estimate + c(-multiplier, multiplier) * stats::sd(x[["stat"]])

  make_ci_df(ci_vec)
}

ci_bias_corrected <- function(x, level, point_estimate) {
  point_estimate <- check_obs_stat(point_estimate)

  p <- mean(x[["stat"]] <= point_estimate)
  z0 <- stats::qnorm(p)
  # z_alpha_2 is z_(alpha/2)
  z_alpha_2 <- stats::qnorm((1 + c(-level, level)) / 2)
  new_probs <- stats::pnorm(2*z0 + z_alpha_2)

  ci_vec <- stats::quantile(x[["stat"]], probs = new_probs)

  make_ci_df(ci_vec)
}

check_ci_args <- function(x, level, type, point_estimate){
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
      'A numeric value needs to be given for `point_estimate` ',
      'for `type` "se" or "bias-corrected".'
    )
  }
}

make_ci_df <- function(ci_vec) {
  tibble::tibble(lower_ci = ci_vec[[1]], upper_ci = ci_vec[[2]])
}
