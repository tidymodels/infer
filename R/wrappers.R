# Wrapper functions
# Different shortcuts to doing traditional hypothesis tests & confidence intervals
# in R as well as calculating test statistics,
# following a pipe-able framework

#'
#' A tidier version of t.test for two sample tests
#'
#' @param data a data frame that can be coerced into a \code{\link[tibble]{tibble}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param alternative character string specifying the direction of the alternative hypothesis. Options are
#' "\code{two_sided}" (default), "\code{greater}", or "\code{less}".
#' @param ... currently ignored
#' @export
#' @examples
#' # t test for comparing mpg against automatic/manual
#'   mtcars %>%
#'     dplyr::mutate(am = factor(am)) %>%
#'     t_test(mpg ~ am, alternative = "less")


t_test <- function(data, formula, #response = NULL, explanatory = NULL,
                   alternative = "two_sided",...){

  # Match with old "dot" syntax
  if(alternative == "two_sided")
    alternative <- "two.sided"

  ### Only currently working with formula interface
#  if (hasArg(formula)) {
    data %>%
      stats::t.test(formula = formula, data = .,
                    alternative = alternative) %>%
      broom::glance() %>%
      dplyr::select(statistic, t_df = parameter, p_value = p.value,
                    alternative)
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
#' @param ... currently ignored
#' @export

t_stat <- function(data, formula, ...){
  data %>%
    t_test(formula = formula, ...) %>%
    dplyr::select(statistic) %>%
    dplyr::pull()
}

#'
#' A tidier version of chisq.test for two sample tests
#'
#' @param data a data frame that can be coerced into a \code{\link[tibble]{tibble}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param ... currently ignored
#' @importFrom rlang f_lhs f_rhs
#' @export
#' @examples
#' # chisq test for comparing number of cylinders against automatic/manual
#'   mtcars %>%
#'     dplyr::mutate(cyl = factor(cyl), am = factor(am)) %>%
#'     chisq_test(cyl ~ am)

chisq_test <- function(data, formula, #response = NULL, explanatory = NULL,
                       ...){

  ## Only currently working with formula interface
  explanatory_var <- f_rhs(formula)
  response_var <- f_lhs(formula)

  stats::chisq.test(x = table(data[[as.character(explanatory_var)]],
                            data[[as.character(response_var)]])) %>%
    broom::glance() %>%
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)
}

#' A shortcut wrapper function to get the observed test statistic for a chisq test. Uses \code{stats::chisq.test}, which applies a continuity correction.
#'
#' @param data a data frame that can be coerced into a \code{\link[tibble]{tibble}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param ... currently ignored
#' @export

chisq_stat <- function(data, formula, ...){
  data %>%
    chisq_test(formula = formula, ...) %>%
    dplyr::select(statistic) %>%
    dplyr::pull()
}
