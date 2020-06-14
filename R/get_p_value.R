#' Compute p-value
#'
#' @description
#'
#' Compute a p-value from a null distribution and observed statistic. 
#' Simulation-based methods are (currently only) supported.
#' 
#' Learn more in `vignette("infer")`.
#'
#' @param x Data frame of calculated statistics as returned by [generate()]
#' @param obs_stat A numeric value or a 1x1 data frame (as extreme or more
#'   extreme than this).
#' @param direction A character string. Options are `"less"`, `"greater"`, or
#'   `"two-sided"`. Can also use `"left"`, `"right"`, `"both"`, 
#'   `"two_sided"`, or `"two sided"`.
#'
#' @return A 1x1 [tibble][tibble::tibble] with value between 0 and 1.
#'
#' @section Aliases:
#' `get_pvalue()` is an alias of `get_p_value()`.
#' `p_value` is a deprecated alias of `get_p_value()`.
#' 
#' @section Zero p-value:
#' Though a true p-value of 0 is impossible, `get_p_value()` may return 0 in 
#' some cases. This is due to the simulation-based nature of the \{infer\} 
#' package; the output of this function is an approximation based on 
#' the number of `reps` chosen in the `generate()` step. When the observed
#' statistic is very unlikely given the null hypothesis, and only a small
#' number of `reps` have been generated to form a null distribution,
#' it is possible that the observed statistic will be more extreme than
#' every test statistic generated to form the null distribution, resulting
#' in an approximate p-value of 0. In this case, the true p-value is a small 
#' value likely less than `3/reps` (based on a poisson approximation).
#' 
#' In the case that a p-value of zero is reported, a warning message will be 
#' raised to caution the user against reporting a p-value exactly equal to 0.
#' 
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
#    # calculate the p-value for the point estimate
#'   get_p_value(obs_stat = point_estimate, direction = "two-sided")
#'   
#' # More in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }  
#'   
#' @name get_p_value
NULL

#' @rdname get_p_value
#' @export
get_p_value <- function(x, obs_stat, direction) {
  check_type(x, is.data.frame)
  if (!is_generated(x) & is_hypothesized(x)) {
    stop_glue(
      "Theoretical p-values are not yet supported.",
      "`x` should be the result of calling `generate()`.",
      .sep = " "
    )
  }
  check_for_nan(x, "get_p_value")
  obs_stat <- check_obs_stat(obs_stat)
  check_direction(direction)

  simulation_based_p_value(x = x, obs_stat = obs_stat, direction = direction)

  ## Theoretical-based p-value
  # Could be more specific
  # else if(is.null(attr(x, "theory_type")) || is.null(attr(x, "distr_param")))
  #   stop_glue("Attributes have not been set appropriately. ",
  #             "Check your {{infer}} pipeline again.")

  # if(!("stat" %in% names(x))){
  #    # Theoretical distribution
  #  which_distribution(x,
  #                     theory_type <- attr(x, "theory_type"),
  #                     obs_stat = obs_stat,
  #                     direction = direction)
  # }
}

#' @rdname get_p_value
#' @export
get_pvalue <- function(x, obs_stat, direction) {
  get_p_value(x = x, obs_stat = obs_stat, direction = direction)
}

simulation_based_p_value <- function(x, obs_stat, direction) {
  if (direction %in% c("less", "left")) {
    pval <- left_p_value(x[["stat"]], obs_stat)
  } else if (direction %in% c("greater", "right")) {
    pval <- right_p_value(x[["stat"]], obs_stat)
  } else {
    pval <- two_sided_p_value(x[["stat"]], obs_stat)
  }
  
  if (abs(pval) < 1e-16) {
    warning_glue(
      "Please be cautious in reporting a p-value of 0. This result is an ",
      "approximation based on the number of `reps` chosen in the `generate()` ",
      "step. See `?get_p_value()` for more information."
    )
  }

  tibble::tibble(p_value = pval)
}

left_p_value <- function(vec, obs_stat) {
  mean(vec <= obs_stat)
}

right_p_value <- function(vec, obs_stat) {
  mean(vec >= obs_stat)
}

two_sided_p_value <- function(vec, obs_stat) {
  left_pval <- left_p_value(vec, obs_stat)
  right_pval <- right_p_value(vec, obs_stat)
  raw_res <- 2 * min(left_pval, right_pval)
  
  min(raw_res, 1)
}

is_generated <- function(x) {
  !is.null(attr(x, "generate")) && attr(x, "generate")
}

# which_distribution <- function(x, theory_type, obs_stat, direction){
#
#   param <- attr(x, "distr_param")
#   if(!is.null(attr(x, "distr_param2")))
#     param2 <- attr(x, "distr_param2")
#
#   if(theory_type == "Two sample t")
#     return(
#       pt(q = obs_stat,
#          df = param,
#          lower.tail = set_lower_tail(direction)
#         )
#     )
# }

#theory_t_pvalue <-

# set_lower_tail <- function(direction){
#   if(direction %in% c("greater", "right"))
#     lower_tail <- FALSE
#   else
#     lower_tail <- TRUE
#
#   lower_tail
# }
