# Wrapper functions
# Different shortcuts to doing traditional hypothesis tests & confidence
# intervals in R as well as calculating test statistics, following a pipe-able
# framework

#' Tidy t-test
#'
#' A tidier version of [t.test()][stats::t.test()] for two sample tests.
#'
#' @param data A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right.
#' @param order A string vector of specifying the order in which the levels of
#'   the explanatory variable should be ordered for subtraction, where `order =
#'   c("first", "second")` means `("first" - "second")`.
#' @param alternative Character string giving the direction of the alternative
#'   hypothesis. Options are `"two_sided"` (default), `"greater"`, or `"less"`.
#' @param mu A numeric value giving the hypothesized null mean value for a one
#'   sample test and the hypothesized difference for a two sample test.
#' @param conf_int A logical value for whether to include the confidence
#'   interval or not. `TRUE` by default.
#' @param conf_level A numeric value between 0 and 1. Default value is 0.95.
#' @param ... For passing in other arguments to [t.test()][stats::t.test()].
#'
#' @examples
#' # t test for comparing mpg against automatic/manual
#' mtcars %>%
#'   dplyr::mutate(am = factor(am)) %>%
#'   t_test(mpg ~ am, order = c("1", "0"), alternative = "less")
#'
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @export
t_test <- function(data, formula, # response = NULL, explanatory = NULL,
                   order = NULL,
                   alternative = "two_sided", mu = 0,
                   conf_int = TRUE,
                   conf_level = 0.95,
                   ...) {
  check_conf_level(conf_level)

  # Match with old "dot" syntax
  if (alternative == "two_sided") {
    alternative <- "two.sided"
  }

  ### Only currently working with formula interface
#  if (hasArg(formula)) {
  if (!is.null(f_rhs(formula))) {
    data[[as.character(f_rhs(formula))]] <-
      factor(data[[as.character(f_rhs(formula))]],
             levels = c(order[1], order[2]))

    # Two sample case
    prelim <- data %>%
      stats::t.test(formula = formula, data = .,
                    alternative = alternative,
                    mu = mu,
                    conf.level = conf_level,
                    ...) %>%
      broom::glance()
  } else {
    # One sample case
    # To fix weird indexing error convert back to data.frame
    # (Error: Can't use matrix or array for column indexing)
    data <- as.data.frame(data)
    prelim <- stats::t.test(x = data[[as.character(f_lhs(formula))]],
                  alternative = alternative,
                  mu = mu,
                  conf.level = conf_level,
                  ...) %>%
      broom::glance()
  }

  if (conf_int) {
    results <- prelim %>%
      dplyr::select(statistic, t_df = parameter, p_value = p.value,
                    alternative,
                    lower_ci = conf.low,
                    upper_ci = conf.high)
  } else {
    results <- prelim %>%
      dplyr::select(statistic, t_df = parameter, p_value = p.value,
                    alternative)
  }

    return(results)
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

#' Tidy t-test statistic
#'
#' A shortcut wrapper function to get the observed test statistic for a t test.
#'
#' @param data A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right.
#' @param ... Pass in arguments to \\{infer\\} functions.
#'
#' @export
t_stat <- function(data, formula, ...) {
  data %>%
    t_test(formula = formula, ...) %>%
    dplyr::select(statistic)
}

#' Tidy chi-squared test
#'
#' A tidier version of [chisq.test()][stats::chisq.test()] for goodness of fit
#' tests and tests of independence.
#'
#' @param data A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right.
#' @param ... Additional arguments for [chisq.test()][stats::chisq.test()].
#'
#' @examples
#' # chisq test for comparing number of cylinders against automatic/manual
#' mtcars %>%
#'   dplyr::mutate(cyl = factor(cyl), am = factor(am)) %>%
#'   chisq_test(cyl ~ am)
#'
#' @importFrom rlang f_lhs f_rhs
#' @export
chisq_test <- function(data, formula, # response = NULL, explanatory = NULL,
                       ...) {
  if (is.null(f_rhs(formula))) {
    stop_glue(
      "`chisq_test()` currently only has functionality for ",
      "Chi-Square Test of Independence, not for Chi-Square Goodness of Fit."
    )
  }
  ## Only currently working with formula interface
  explanatory_var <- f_rhs(formula)
  response_var <- f_lhs(formula)

  df <- data[, as.character(c(response_var, explanatory_var))]
  stats::chisq.test(table(df), ...) %>%
    broom::glance() %>%
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)
}

#' Tidy chi-squared test statistic
#'
#' A shortcut wrapper function to get the observed test statistic for a chisq
#' test. Uses [chisq.test()][stats::chisq.test()], which applies a continuity
#' correction.
#'
#' @param data A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right.
#' @param ... Additional arguments for [chisq.test()][stats::chisq.test()].
#'
#' @export
chisq_stat <- function(data, formula, ...) {
  if (is.null(f_rhs(formula))) {
    stop_glue(
      "`chisq_stat()` currently only has functionality for ",
      "Chi-Square Test of Independence, not for Chi-Square Goodness of Fit. ",
      "Use `specify() %>% hypothesize() %>% calculate()` instead."
    )
  } else {
    data %>%
      specify(formula = formula, ...) %>%
      calculate(stat = "Chisq")
  }
}

check_conf_level <- function(conf_level) {
  if (class(conf_level) != "numeric" |
      conf_level < 0 |
      conf_level > 1) {
    stop_glue("The `conf_level` argument must be a number between 0 and 1.")
  }
}
