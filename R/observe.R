#' Calculate observed statistics
#'
#' @description
#'
#' This function is a wrapper that calls [specify()], [hypothesize()], and
#' [calculate()] consecutively that can be used to calculate observed 
#' statistics from data. [hypothesize()] will only be called if a point
#' null hypothesis parameter is supplied.
#'
#' Learn more in `vignette("infer")`.
#'
#' @inheritParams specify
#' @inheritParams hypothesize
#' @inheritParams calculate
#'
#' @return A 1-column tibble containing the calculated statistic `stat`.
#'
#' @examples
#' # calculating the observed mean number of hours worked per week
#' gss %>%
#'   observe(hours ~ NULL, stat = "mean")
#' 
#' # equivalently, calculating the same statistic with the core verbs
#' gss %>%
#'   specify(response = hours) %>%
#'   calculate(stat = "mean")
#' 
#' # calculating a t statistic for hypothesized mu = 40 hours worked/week
#' gss %>%
#'   observe(hours ~ NULL, stat = "t", null = "point", mu = 40)
#' 
#' # equivalently, calculating the same statistic with the core verbs
#' gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40) %>%
#'   calculate(stat = "t")
#' 
#' # similarly for a difference in means in age based on whether
#' # the respondent has a college degree
#' observe(
#'   gss,
#'   age ~ college,
#'   stat = "diff in means",
#'   order = c("degree", "no degree")
#' )
#' 
#' # equivalently, calculating the same statistic with the core verbs
#' gss %>%
#'   specify(age ~ college) %>%
#'   calculate("diff in means", order = c("degree", "no degree"))
#'   
#' # for a more in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @export
observe <- function(
    x,
    # specify arguments
    formula, response = NULL, explanatory = NULL, success = NULL,
    # hypothesize arguments
    null = NULL, p = NULL, mu = NULL, med = NULL, sigma = NULL,
    # calculate arguments
    stat = c("mean", "median", "sum", "sd", "prop", "count", "diff in means", 
             "diff in medians", "diff in props", "Chisq", "F", "slope", 
             "correlation", "t", "z", "ratio of props", "odds ratio"),
    order = NULL,
    ...) {
    
  # use hypothesize() if appropriate (or needed to pass an informative
  # message/warning). otherwise, pipe directly to calculate().
  if (!all(sapply(list(p, mu, med, sigma), is.null))) {
    hypothesize_fn <- hypothesize
  } else {
    hypothesize_fn <- function(x, ...) {x}
  }
  
  # pass arguments on to core verbs
  specify(
    x = x, 
    formula = formula, 
    response = {{response}}, 
    explanatory = {{explanatory}}, 
    success = success
  ) %>%
  hypothesize_fn(
    null = if (has_explanatory(.)) {"independence"} else {"point"}, 
    p = p, 
    mu = mu, 
    med = med, 
    sigma = sigma
  ) %>%
  calculate(
    stat = stat, 
    order = order, 
    ...
  )
}
