#' Compute confidence interval
#'
#' @description
#'
#' Compute a confidence interval around a summary statistic. Only 
#' simulation-based methods are (currently only) supported.
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
#'   corresponding to (multiplier * standard error) as the other option.
#' @param point_estimate A numeric value or a 1x1 data frame set to `NULL` by
#'   default. Needed to be provided if `type = "se"`.
#'
#' @return A 1 x 2 tibble with values corresponding to lower and upper values in
#'   the confidence interval.
#'
#' @section Aliases:
#' `get_ci()` is an alias of `get_confidence_interval()`.
#' `conf_int()` is a deprecated alias of `get_confidence_interval()`.
#'
#' @examples
#' 
#' # find the point estimate---mean number of hours worked per week
#' point_estimate <- gss %>%
#'   specify(response = hours) %>%
#'   calculate(stat = "mean") %>%
#'   dplyr::pull()
#' 
#' # starting with the gss dataset
#' gss %>%
#'   # ...we're interested in the number of hours worked per week
#'   specify(response = hours) %>%
#'   # hypothesizing that the mean is 40
#'   hypothesize(null = "point", mu = 40) %>%
#'   # generating data points for a null distribution
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   # finding the null distribution
#'   calculate(stat = "mean") %>%
#    # calculate the confidence interval around the point estimate
#'   get_confidence_interval(
#'     point_estimate = point_estimate,
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
  
  switch(
    type,
    percentile = ci_percentile(x, level),
    se = ci_se(x, level, point_estimate)
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
  
  tibble::as_tibble(as.list(ci_vec))
}

ci_se <- function(x, level, point_estimate) {
  point_estimate <- check_obs_stat(point_estimate)
  
  multiplier <- stats::qnorm((1 + level) / 2)
  ci_vec <- point_estimate + c(-multiplier, multiplier) * stats::sd(x[["stat"]])
  
  tibble::tibble(lower = ci_vec[[1]], upper = ci_vec[[2]])
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

  if (!(type %in% c("percentile", "se"))) {
    stop_glue('The options for `type` are "percentile" or "se".')
  }

  if ((type == "se") && is.null(point_estimate)) {
    stop_glue(
      'A numeric value needs to be given for `point_estimate` ',
      'for `type = "se"'
    )
  }
}

