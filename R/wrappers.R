# Wrapper functions
# Different shortcuts to doing traditional hypothesis tests & confidence
# intervals in R as well as calculating test statistics, following a pipe-able
# framework

#' Tidy t-test
#'
#' @description
#'
#' A tidier version of [t.test()][stats::t.test()] for two sample tests.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right.
#' @param response The variable name in `x` that will serve as the response.
#'   This is alternative to using the `formula` argument.
#' @param explanatory The variable name in `x` that will serve as the
#'   explanatory variable.
#' @param order A string vector of specifying the order in which the levels of
#'   the explanatory variable should be ordered for subtraction, where `order =
#'   c("first", "second")` means `("first" - "second")`.
#' @param alternative Character string giving the direction of the alternative
#'   hypothesis. Options are `"two-sided"` (default), `"greater"`, or `"less"`.
#' @param mu A numeric value giving the hypothesized null mean value for a one
#'   sample test and the hypothesized difference for a two sample test.
#' @param conf_int A logical value for whether to include the confidence
#'   interval or not. `TRUE` by default.
#' @param conf_level A numeric value between 0 and 1. Default value is 0.95.
#' @param ... For passing in other arguments to [t.test()][stats::t.test()].
#'
#' @examples
#' library(tidyr)
#'
#' # t test for number of hours worked per week
#' # by college degree status
#' gss %>%
#'    tidyr::drop_na(college) %>%
#'    t_test(formula = hours ~ college,
#'       order = c("degree", "no degree"),
#'       alternative = "two-sided")
#'
#' # see vignette("infer") for more explanation of the
#' # intuition behind the infer package, and vignette("t_test")
#' # for more examples of t-tests using infer
#'
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @importFrom stats as.formula
#' @export
t_test <- function(x, formula,
                   response = NULL,
                   explanatory = NULL,
                   order = NULL,
                   alternative = "two-sided",
                   mu = 0,
                   conf_int = TRUE,
                   conf_level = 0.95,
                   ...) {
  check_conf_level(conf_level)

  # convert all character and logical variables to be factor variables
  x <- tibble::as_tibble(x) %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.logical, as.factor)

  # parse response and explanatory variables
  response    <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- parse_variables(x = x, formula = formula,
                       response = response, explanatory = explanatory)

  # match with old "dot" syntax in t.test
  if (alternative %in% c("two-sided", "two_sided", "two sided")) {
    alternative <- "two.sided"
  }

  # two sample
  if (has_explanatory(x)) {
    # if (!is.null(order)) {
    #   x[[as.character(attr(x, "explanatory"))]] <- factor(explanatory_variable(x),
    #                                                       levels = c(order[1],
    #                                                                  order[2]),
    #                                                       ordered = TRUE)
    # }
    order <- check_order(x, explanatory_variable(x), order, in_calculate = FALSE)
    x <- reorder_explanatory(x, order)
    prelim <- stats::t.test(formula = as.formula(paste0(attr(x, "response"),
                                                        " ~ ",
                                                        attr(x, "explanatory"))),
                            data = x,
                            alternative = alternative,
                            mu = mu,
                            conf.level = conf_level,
                            ...) %>%
      broom::glance()
  } else { # one sample
    prelim <- stats::t.test(response_variable(x),
                            alternative = alternative,
                            mu = mu,
                            conf.level = conf_level) %>%
      broom::glance()
  }

  if (conf_int) {
    results <- prelim %>%
      dplyr::select(
        statistic, t_df = parameter, p_value = p.value, alternative,
        lower_ci = conf.low, upper_ci = conf.high
      )
  } else {
    results <- prelim %>%
      dplyr::select(
        statistic, t_df = parameter, p_value = p.value, alternative
      )
  }

  results
}

#' Tidy t-test statistic
#'
#' @description
#'
#' A shortcut wrapper function to get the observed test statistic for a t test.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right.
#' @param response The variable name in `x` that will serve as the response.
#'   This is alternative to using the `formula` argument.
#' @param explanatory The variable name in `x` that will serve as the
#'   explanatory variable.
#' @param order A string vector of specifying the order in which the levels of
#'   the explanatory variable should be ordered for subtraction, where `order =
#'   c("first", "second")` means `("first" - "second")`.
#' @param alternative Character string giving the direction of the alternative
#'   hypothesis. Options are `"two-sided"` (default), `"greater"`, or `"less"`.
#' @param mu A numeric value giving the hypothesized null mean value for a one
#'   sample test and the hypothesized difference for a two sample test.
#' @param conf_int A logical value for whether to include the confidence
#'   interval or not. `TRUE` by default.
#' @param conf_level A numeric value between 0 and 1. Default value is 0.95.
#' @param ... Pass in arguments to \\{infer\\} functions.
#'
#' @examples
#' library(tidyr)
#'
#' # t test statistic for true mean number of hours worked
#' # per week of 40
#' gss %>%
#'    t_stat(response = hours, mu = 40)
#'
#' # t test statistic for number of hours worked per week
#' # by college degree status
#' gss %>%
#'    tidyr::drop_na(college) %>%
#'    t_stat(formula = hours ~ college,
#'       order = c("degree", "no degree"),
#'       alternative = "two-sided")
#'
#' @export
t_stat <- function(x, formula,
                   response = NULL,
                   explanatory = NULL,
                   order = NULL,
                   alternative = "two-sided",
                   mu = 0,
                   conf_int = FALSE,
                   conf_level = 0.95,
                   ...) {
  check_conf_level(conf_level)

  # convert all character and logical variables to be factor variables
  x <- tibble::as_tibble(x) %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.logical, as.factor)

  # parse response and explanatory variables
  response    <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- parse_variables(x = x, formula = formula,
                       response = response, explanatory = explanatory)

  # match with old "dot" syntax in t.test
  if (alternative %in% c("two-sided", "two_sided", "two sided")) {
    alternative <- "two.sided"
  }

  # two sample
  if (has_explanatory(x)) {
    # if (!is.null(order)) {
    #   x[[as.character(attr(x, "explanatory"))]] <- factor(explanatory_variable(x),
    #                                                       levels = c(order[1],
    #                                                                  order[2]),
    #                                                       ordered = TRUE)
    # }
    order <- check_order(x, explanatory_variable(x), order, in_calculate = FALSE)
    x <- reorder_explanatory(x, order)
    prelim <- stats::t.test(formula = as.formula(paste0(attr(x, "response"),
                                                        " ~ ",
                                                        attr(x, "explanatory"))),
                            data = x,
                            alternative = alternative,
                            mu = mu,
                            conf.level = conf_level,
                            ...) %>%
      broom::glance()
  } else { # one sample
    prelim <- stats::t.test(response_variable(x),
                            alternative = alternative,
                            mu = mu,
                            conf.level = conf_level) %>%
      broom::glance()
  }

  # removed unnecessary if(conf_int) clause; only the statistic itself
  # was returned regardless
  results <- prelim %>%
    dplyr::select(statistic) %>%
    pull()

  results
}

#' Tidy chi-squared test
#'
#' @description
#'
#' A tidier version of [chisq.test()][stats::chisq.test()] for goodness of fit
#' tests and tests of independence.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right.
#' @param response The variable name in `x` that will serve as the response.
#'   This is alternative to using the `formula` argument.
#' @param explanatory The variable name in `x` that will serve as the
#'   explanatory variable.
#' @param ... Additional arguments for [chisq.test()][stats::chisq.test()].
#'
#' @examples
#' # chi-squared test of independence for college completion
#' # status depending on one's self-identified income class
#' chisq_test(gss, college ~ finrela)
#'
#' # chi-squared goodness of fit test on whether self-identified
#' # income class follows a uniform distribution
#' chisq_test(gss,
#'            response = finrela,
#'            p = c("far below average" = 1/6,
#'                  "below average" = 1/6,
#'                  "average" = 1/6,
#'                  "above average" = 1/6,
#'                  "far above average" = 1/6,
#'                  "DK" = 1/6))
#'
#' @export
chisq_test <- function(x, formula, response = NULL,
                       explanatory = NULL, ...) {
  # Parse response and explanatory variables
  response    <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- parse_variables(x = x, formula = formula,
                       response = response, explanatory = explanatory)

  if (!(class(response_variable(x)) %in% c("logical", "character", "factor"))) {
    stop_glue(
      'The response variable of `{attr(x, "response")}` is not appropriate\n',
      "since the response variable is expected to be categorical."
    )
  }
  if (has_explanatory(x) &&
      !(class(explanatory_variable(x)) %in% c("logical", "character", "factor"))) {
    stop_glue(
      'The explanatory variable of `{attr(x, "explanatory")}` is not appropriate\n',
      "since the explanatory variable is expected to be categorical."
    )
  }

  x <- x %>%
    select(one_of(c(
      as.character((attr(x, "response"))), as.character(attr(x, "explanatory"))
    ))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.logical, as.factor)

  stats::chisq.test(table(x), ...) %>%
    broom::glance() %>%
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)
}

#' Tidy chi-squared test statistic
#'
#'  @description
#'
#' A shortcut wrapper function to get the observed test statistic for a chisq
#' test. Uses [chisq.test()][stats::chisq.test()], which applies a continuity
#' correction.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right.
#' @param response The variable name in `x` that will serve as the response.
#'   This is alternative to using the `formula` argument.
#' @param explanatory The variable name in `x` that will serve as the
#'   explanatory variable.
#' @param ... Additional arguments for [chisq.test()][stats::chisq.test()].
#'
#' @examples
#' # chi-squared test statistic for test of independence
#' # of college completion status depending and one's
#' # self-identified income class
#' chisq_stat(gss, college ~ finrela)
#'
#' # chi-squared test statistic for a goodness of fit
#' # test on whether self-identified income class
#' # follows a uniform distribution
#' chisq_stat(gss,
#'            response = finrela,
#'            p = c("far below average" = 1/6,
#'                  "below average" = 1/6,
#'                  "average" = 1/6,
#'                  "above average" = 1/6,
#'                  "far above average" = 1/6,
#'                  "DK" = 1/6))
#'
#' @export
chisq_stat <- function(x, formula, response = NULL,
                       explanatory = NULL, ...) {
  # Parse response and explanatory variables
  response    <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- parse_variables(x = x, formula = formula,
                       response = response, explanatory = explanatory)

  if (!(class(response_variable(x)) %in% c("logical", "character", "factor"))) {
    stop_glue(
      'The response variable of `{attr(x, "response")}` is not appropriate\n',
      "since the response variable is expected to be categorical."
    )
  }
  if (has_explanatory(x) &&
      !(class(explanatory_variable(x)) %in% c("logical", "character", "factor"))) {
    stop_glue(
      'The explanatory variable of `{attr(x, "explanatory")}` is not appropriate\n',
      "since the response variable is expected to be categorical."
    )
  }

  x <- x %>%
    select(one_of(c(
      as.character((attr(x, "response"))), as.character(attr(x, "explanatory"))
    ))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.logical, as.factor)

  suppressWarnings(stats::chisq.test(table(x), ...)) %>%
    broom::glance() %>%
    dplyr::select(statistic) %>%
    pull()
}

check_conf_level <- function(conf_level) {
  if (
    (class(conf_level) != "numeric") | (conf_level < 0) | (conf_level > 1)
  ) {
    stop_glue("The `conf_level` argument must be a number between 0 and 1.")
  }
}

#' Tidy proportion test
#'
#' @description
#'
#' A tidier version of [prop.test()][stats::prop.test()] for equal or given
#' proportions.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right, where an explanatory variable NULL indicates
#'   a test of a single proportion.
#' @param response The variable name in `x` that will serve as the response.
#'   This is alternative to using the `formula` argument. This is an alternative
#'   to the formula interface.
#' @param explanatory The variable name in `x` that will serve as the
#'   explanatory variable. Optional. This is an alternative to the formula
#'   interface.
#' @param order A string vector specifying the order in which the proportions
#'   should be subtracted, where  `order = c("first", "second")` means
#'   `"first" - "second"`. Ignored for one-sample tests, and optional for two
#'   sample tests.
#' @param alternative Character string giving the direction of the alternative
#'   hypothesis. Options are `"two-sided"` (default), `"greater"`, or `"less"`.
#'   Only used when testing the null that a single proportion equals a given
#'   value, or that two proportions are equal; ignored otherwise.
#' @param p A numeric vector giving the hypothesized null proportion of
#' success for each group.
#' @param conf_int A logical value for whether to report the confidence
#'   interval or not. `TRUE` by default, ignored if `p` is specified for a
#'   two-sample test. Only used when testing the null that a single
#'   proportion equals a given value, or that two proportions are equal;
#'   ignored otherwise.
#' @param conf_level A numeric value between 0 and 1. Default value is 0.95.
#'   Only used when testing the null that a single proportion equals a given
#'   value, or that two proportions are equal; ignored otherwise.
#' @param success The level of `response` that will be considered a success, as
#'   a string. Only used when testing the null that a single
#'   proportion equals a given value, or that two proportions are equal;
#'   ignored otherwise.
#' @param ... Additional arguments for [prop.test()][stats::prop.test()].
#'
#' @examples
#' # two-sample proportion test for difference in proportions of
#' # college completion by respondent sex
#' prop_test(gss,
#'           college ~ sex,
#'           order = c("female", "male"))
#'
#' # one-sample proportion test for hypothesized null
#' # proportion of college completion of .2
#' prop_test(gss,
#'           college ~ NULL,
#'           p = .2)
#'
#' @export
prop_test <- function(x, formula,
                      response = NULL,
                      explanatory = NULL,
                      p = NULL,
                      order = NULL,
                      alternative = "two-sided",
                      conf_int = TRUE,
                      conf_level = 0.95,
                      success = NULL,
                      ...) {
  # Parse response and explanatory variables
  response    <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- parse_variables(x = x, formula = formula,
                       response = response, explanatory = explanatory)

  if (!(class(response_variable(x)) %in% c("logical", "character", "factor"))) {
    stop_glue(
      'The response variable of `{attr(x, "response")}` is not appropriate\n',
      "since the response variable is expected to be categorical."
    )
  }
  if (has_explanatory(x) &&
      !(class(explanatory_variable(x)) %in% c("logical", "character", "factor"))) {
    stop_glue(
      'The explanatory variable of `{attr(x, "explanatory")}` is not appropriate\n',
      "since the explanatory variable is expected to be categorical."
    )
  }
  # match with old "dot" syntax in t.test
  if (alternative %in% c("two-sided", "two_sided", "two sided")) {
    alternative <- "two.sided"
  }

  # process "success" arg
  lvls <- levels(factor(response_variable(x)))

  if (!is.null(success)) {
    if (!is.character(success)) {
      stop_glue("`success` must be a string.")
    }
    if (!(success %in% lvls)) {
      stop_glue('{success} is not a valid level of {attr(x, "response")}.')
    }
    lvls <- c(success, lvls[lvls != success])
  } else {
    success <- lvls[1]
  }

  # two sample
  if (has_explanatory(x)) {

    order <- check_order(x, explanatory_variable(x), order, in_calculate = FALSE)

    # make a summary table to supply to prop.test
    sum_table <- x %>%
      select(as.character((attr(x, "response"))),
             as.character(attr(x, "explanatory"))) %>%
      mutate_if(is.character, as.factor) %>%
      mutate_if(is.logical, as.factor) %>%
      table()

    # reorder according to the order and success arguments
    sum_table <- sum_table[lvls, order]

    prelim <- stats::prop.test(x = sum_table,
                               alternative = alternative,
                               conf.level = conf_level,
                               p = p,
                               ...) %>%
      broom::glance()
  } else { # one sample
    response_tbl <- response_variable(x) %>%
      factor() %>%
      relevel(success) %>%
      table()

    if (is.null(p)) {
      message_glue(
        "No `p` argument was hypothesized, so the test will ",
        "assume a null hypothesis `p = .5`."
      )
    }

    prelim <- stats::prop.test(x = response_tbl,
                               alternative = alternative,
                               conf.level = conf_level,
                               p = p,
                               ...) %>%
      broom::glance()
  }

  if (conf_int & is.null(p) & (prelim$parameter <= 2)) {
     results <- prelim %>%
       dplyr::select(statistic,
                     chisq_df = parameter,
                     p_value = p.value,
                     alternative,
                     lower_ci = conf.low,
                     upper_ci = conf.high)

  } else if (prelim$parameter <= 2) {
     results <- prelim %>%
       dplyr::select(statistic,
                     chisq_df = parameter,
                     p_value = p.value,
                     alternative)
  } else {
    results <- prelim %>%
      dplyr::select(statistic,
                    chisq_df = parameter,
                    p_value = p.value)
  }

  results
}

