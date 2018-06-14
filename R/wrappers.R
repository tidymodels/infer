# Wrapper functions
# Different shortcuts to doing traditional hypothesis tests & confidence intervals
# in R as well as calculating test statistics,
# following a pipe-able framework

#'
#' A tidier version of t.test for two sample tests
#'
#' @param data a data frame that can be coerced into a \code{\link[tibble]{tibble}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param order #' @param order a string vector of specifying the order in which the levels of
#' the explanatory variable should be ordered for subtraction, where
#' \code{order = c("first", "second")} means \code{("first" - "second")}
#' @param alternative character string giving the direction of the alternative hypothesis. Options are
#' "\code{two_sided}" (default), "\code{greater}", or "\code{less}".
#' @param mu a numeric value giving the hypothesized null mean value for a one sample test
#' and the hypothesized difference for a two sample test
#' @param ... currently ignored
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @export
#' @examples
#' # t test for comparing mpg against automatic/manual
#'   mtcars %>%
#'     dplyr::mutate(am = factor(am)) %>%
#'     t_test(mpg ~ am, order = c("1", "0"), alternative = "less")


t_test <- function(data, formula, #response = NULL, explanatory = NULL,
                   order = NULL,
                   alternative = "two_sided", mu = 0, 
                   ...){

  # Match with old "dot" syntax
  if(alternative == "two_sided")
    alternative <- "two.sided"

  ### Only currently working with formula interface
#  if (hasArg(formula)) {
  if(!is.null(f_rhs(formula))){
    
    data[[as.character(f_rhs(formula))]] <-
      factor(data[[as.character(f_rhs(formula))]],
             levels = c(order[1], order[2]))
    
    # Two sample case
    data %>%
      stats::t.test(formula = formula, data = .,
                    alternative = alternative,
                    mu = mu, ...) %>%
      broom::glance() %>%
      dplyr::select(statistic, t_df = parameter, p_value = p.value,
                    alternative)
  } else {
    # One sample case
    # To fix weird indexing error convert back to data.frame
    # (Error: Can't use matrix or array for column indexing)
    data <- as.data.frame(data)
    results <- stats::t.test(data[[as.character(f_lhs(formula))]],
                  alternative = alternative,
                  mu = mu, ...) %>% 
      broom::glance() %>%
      dplyr::select(statistic, t_df = parameter, p_value = p.value,
                    alternative)
    return(results)
  }
#  } else {
    # data %>%
    #   stats::t.test(formula = substitute(response) ~ substitute(explanatory),
    #                 data = .,
    #                 alternative = alternative) %>%
    #   broom::glance() %>%
    #   dplyr::select(statistic, t_df = parameter, p_value = p.value,
    #                 alternative)

#     t.test(y = data[[as.character(substitute(response))]],
#            x = data[[as.character(substitute(explanatory))]],
#            alternative = alternative) %>%
#     broom::glance() %>%
#     select(statistic, t_df = parameter, p_value = p.value, alternative)
# }
}

#' A shortcut wrapper function to get the observed test statistic for a t test
#'
#' @param data a data frame that can be coerced into a \code{\link[tibble]{tibble}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param ... pass in arguments to {infer} functions
#' @export

t_stat <- function(data, formula, ...){
  data %>% 
    specify(formula = formula) %>% 
    calculate(stat = "t", ...)
}

#'
#' A tidier version of chisq.test for goodness of fit tests and tests of independence.
#'
#' @param data a data frame that can be coerced into a \code{\link[tibble]{tibble}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param ... additional arguments for \code{chisq.test}
#' @importFrom rlang f_lhs f_rhs
#' @export
#' @examples
#' # chisq test for comparing number of cylinders against automatic/manual
#'   mtcars %>%
#'     dplyr::mutate(cyl = factor(cyl), am = factor(am)) %>%
#'     chisq_test(cyl ~ am)

chisq_test <- function(data, formula, #response = NULL, explanatory = NULL,
                       ...){

  if(is.null(f_rhs(formula)))
    stop(paste("`chisq_test()` currently only has functionality for",
               "Chi-Square Test of Independence, not for Chi-Square",
               "Goodness of Fit. Use `specify() %>% hypothesize()",
               " %>% calculate()` instead."))
  ## Only currently working with formula interface
  explanatory_var <- f_rhs(formula)
  response_var <- f_lhs(formula)

  df <- data[ , as.character(c(response_var, explanatory_var))]
  stats::chisq.test(table(df), ...) %>%
    broom::glance() %>%
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)
}

#' A shortcut wrapper function to get the observed test statistic for a chisq test. Uses \code{stats::chisq.test}, which applies a continuity correction.
#'
#' @param data a data frame that can be coerced into a \code{\link[tibble]{tibble}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param ... additional arguments for \code{chisq.test}
#' @export

chisq_stat <- function(data, formula, ...){
  
  if(is.null(f_rhs(formula))){
    stop(paste("`chisq_stat()` currently only has functionality for",
               "Chi-Square Test of Independence, not for Chi-Square",
               "Goodness of Fit. Use `specify() %>% hypothesize()",
               " %>% calculate()` instead."))
  } else {
    data %>%
      specify(formula = formula, ...) %>%
      calculate(stat = "Chisq")
  }
}
