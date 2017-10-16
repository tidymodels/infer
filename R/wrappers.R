# Wrapper functions
# Different shortcuts to doing traditional hypothesis tests & confidence intervals
# in R as well as calculating test statistics,
# following a pipe-able framework

#' 
#' t_test
#' 
#' @param data a data frame that can be coerced into a \code{\link[tibble]{tibble}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param alternative character string specifying the direction of the alternative hypothesis. Options are 
#' "\code{two_sided}" (default), "\code{greater}", or "\code{less}".
#' @param ... currently ignored
#' @export
#'
t_test <- function(data, formula, #response = NULL, explanatory = NULL, 
                   alternative = "two_sided",...){
  
  # Match with old "dot" syntax
  if(alternative == "two_sided")
    alternative <- "two.sided"
  
  ### Only currently working with formula interface
 # if (hasArg(formula)) {
    data %>%
      stats::t.test(formula = formula, data = ., alternative = alternative) %>%
      broom::glance() %>%
      dplyr::select(statistic, t_df = parameter, p_value = p.value, alternative)
#  } else {
#      t.test(y = data[[as.character(substitute(response))]], 
#             x = data[[as.character(substitute(explanatory))]], 
#             alternative = alternative) %>%
#    data %>%
#      stats::t.test(formula = new_formula(response, explanatory), data = .) %>%
#      broom::glance() %>%
#      select(statistic, t_df = parameter, p_value = p.value, alternative)
#  }
}

#' t_stat
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