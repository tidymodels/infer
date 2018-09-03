#' Compute confidence interval
#'
#' Only simulation-based methods are (currently only) supported.
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
#' @section Aliases:
#' `get_ci()` is an alias of `get_confidence_interval()`.
#' `conf_int` is a deprecated alias of `get_confidence_interval()`.
#'
#' @examples
#' # Prepare the dataset
#' mtcars_df <- mtcars %>%
#'   dplyr::mutate(am = factor(am))
#'
#' # Calculate the difference in means in the dataset
#' d_hat <- mtcars_df %>%
#'   specify(mpg ~ am) %>%
#'   calculate(stat = "diff in means", order = c("1", "0"))
#'
#' # Same calculation on 100 bootstrap replicates
#' bootstrap_distn <- mtcars_df %>%
#'   specify(mpg ~ am) %>%
#'   generate(reps = 100, type = "bootstrap") %>%
#'   calculate(stat = "diff in means", order = c("1", "0"))
#'
#' # Use level to set the confidence level
#' bootstrap_distn %>%
#'   get_confidence_interval(level = 0.9)
#'
#' # To calculate std error, set the type and point estimate
#' bootstrap_distn %>%
#'   get_confidence_interval(type = "se", point_estimate = d_hat)
#' @name get_confidence_interval
#' @export
get_confidence_interval <- function(x, level = 0.95, type = "percentile",
  point_estimate = NULL){

  check_ci_args(x, level, type, point_estimate)

  if(type == "percentile") {
    ci_vec <- stats::quantile(x[["stat"]],
      probs = c((1 - level) / 2, level + (1 - level) / 2))

    ci <- tibble::tibble(ci_vec[1], ci_vec[2])
    names(ci) <- names(ci_vec)
  } else {
    point_estimate <- check_obs_stat(point_estimate)
    multiplier <- stats::qnorm(1 - (1 - level) / 2)
    ci <- tibble::tibble(
      lower = point_estimate - multiplier * stats::sd(x[["stat"]]),
      upper = point_estimate + multiplier * stats::sd(x[["stat"]]))
  }

  return(ci)
}

#' @rdname get_confidence_interval
#' @export
get_ci <- function(x, level = 0.95, type = "percentile",
  point_estimate = NULL) {
  get_confidence_interval(
    x, level = level, type = type, point_estimate = point_estimate
  )
}

check_ci_args <- function(x, level, type, point_estimate){

  if(!is.null(point_estimate)){
    if(!is.data.frame(point_estimate))
      check_type(point_estimate, is.numeric)
    else
      check_type(point_estimate, is.data.frame)
  }
  check_type(x, is.data.frame)
  check_type(level, is.numeric)
  if(level <= 0 || level >= 1){
    stop_glue("The value of `level` must be between 0 and 1 non-inclusive.")
  }

  if(!(type %in% c("percentile", "se"))){
    stop_glue('The options for `type` are "percentile" or "se".')
  }

  if(type == "se" && is.null(point_estimate))
    stop_glue('A numeric value needs to be given for `point_estimate` ',
      'for `type = "se"')

  if(type == "se" && is.vector(point_estimate))
    check_type(point_estimate, is.numeric)
}

