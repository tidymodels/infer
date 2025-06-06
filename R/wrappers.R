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
#' @inheritParams specify
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
#' gss |>
#'    tidyr::drop_na(college) |>
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
#' @importFrom rlang new_formula
#' @importFrom stats as.formula
#' @family wrapper functions
#' @export
t_test <- function(
  x,
  formula,
  response = NULL,
  explanatory = NULL,
  order = NULL,
  alternative = "two-sided",
  mu = 0,
  conf_int = TRUE,
  conf_level = 0.95,
  ...
) {
  check_conf_level(conf_level)

  # convert all character and logical variables to be factor variables
  x <- standardize_variable_types(x)

  # parse response and explanatory variables
  response <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- parse_variables(
    x = x,
    formula = formula,
    response = response,
    explanatory = explanatory
  )

  # match with old "dot" syntax in t.test
  if (alternative %in% c("two-sided", "two_sided", "two sided", "two.sided")) {
    alternative <- "two.sided"
  }

  # two sample
  if (has_explanatory(x)) {
    order <- check_order(x, order, in_calculate = FALSE, stat = NULL)
    x <- reorder_explanatory(x, order)
    prelim <- stats::t.test(
      formula = new_formula(response_expr(x), explanatory_expr(x)),
      data = x,
      alternative = alternative,
      mu = mu,
      conf.level = conf_level,
      ...
    ) |>
      broom::glance()
  } else {
    # one sample
    prelim <- stats::t.test(
      response_variable(x),
      alternative = alternative,
      mu = mu,
      conf.level = conf_level
    ) |>
      broom::glance()
  }

  if (conf_int) {
    results <- prelim |>
      dplyr::select(
        statistic,
        t_df = parameter,
        p_value = p.value,
        alternative,
        estimate,
        lower_ci = conf.low,
        upper_ci = conf.high
      )
  } else {
    results <- prelim |>
      dplyr::select(
        statistic,
        t_df = parameter,
        p_value = p.value,
        alternative,
        estimate
      )
  }

  results
}

#' Tidy t-test statistic
#'
#' @description
#'
#' A shortcut wrapper function to get the observed test statistic for a t test.
#' This function has been deprecated in favor of the more general [observe()].
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @inheritParams specify
#' @param order A string vector of specifying the order in which the levels of
#'   the explanatory variable should be ordered for subtraction, where `order =
#'   c("first", "second")` means `("first" - "second")`.
#' @param mu A numeric value giving the hypothesized null mean value for a one
#'   sample test and the hypothesized difference for a two sample test.
#' @inheritParams t_test
#' @param ... Pass in arguments to infer functions.
#'
#' @examples
#' library(tidyr)
#'
#' # t test statistic for true mean number of hours worked
#' # per week of 40
#' gss |>
#'    t_stat(response = hours, mu = 40)
#'
#' # t test statistic for number of hours worked per week
#' # by college degree status
#' gss |>
#'    tidyr::drop_na(college) |>
#'    t_stat(formula = hours ~ college,
#'       order = c("degree", "no degree"),
#'       alternative = "two-sided")
#'
#' @family wrapper functions
#' @family functions for calculating observed statistics
#' @export
t_stat <- function(
  x,
  formula,
  response = NULL,
  explanatory = NULL,
  order = NULL,
  alternative = "two-sided",
  mu = 0,
  conf_int = FALSE,
  conf_level = 0.95,
  ...
) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "t_stat()",
    with = "observe()"
  )

  check_conf_level(conf_level)

  # convert all character and logical variables to be factor variables
  x <- standardize_variable_types(x)

  # parse response and explanatory variables
  response <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- parse_variables(
    x = x,
    formula = formula,
    response = response,
    explanatory = explanatory
  )

  # match with old "dot" syntax in t.test
  if (alternative %in% c("two-sided", "two_sided", "two sided", "two.sided")) {
    alternative <- "two.sided"
  }

  # two sample
  if (has_explanatory(x)) {
    order <- check_order(x, order, in_calculate = FALSE, stat = NULL)
    x <- reorder_explanatory(x, order)
    prelim <- stats::t.test(
      formula = new_formula(response_expr(x), explanatory_expr(x)),
      data = x,
      alternative = alternative,
      mu = mu,
      conf.level = conf_level,
      ...
    ) |>
      broom::glance()
  } else {
    # one sample
    prelim <- stats::t.test(
      response_variable(x),
      alternative = alternative,
      mu = mu,
      conf.level = conf_level
    ) |>
      broom::glance()
  }

  # removed unnecessary if(conf_int) clause; only the statistic itself
  # was returned regardless
  results <- prelim |>
    dplyr::select(statistic) |>
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
#' @inheritParams specify
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
#' @family wrapper functions
#' @export
chisq_test <- function(x, formula, response = NULL, explanatory = NULL, ...) {
  # Parse response and explanatory variables
  response <- enquo(response)
  explanatory <- enquo(explanatory)

  x <- standardize_variable_types(x)

  x <- parse_variables(
    x = x,
    formula = formula,
    response = response,
    explanatory = explanatory
  )

  if (!(class(response_variable(x)) %in% c("logical", "character", "factor"))) {
    cli_abort(
      'The response variable of `{response_name(x)}` is not appropriate \\
       since the response variable is expected to be categorical.'
    )
  }
  if (
    has_explanatory(x) &&
      !(class(explanatory_variable(x)) %in% c("logical", "character", "factor"))
  ) {
    cli_abort(
      'The explanatory variable of `{explanatory_name(x)}` is not appropriate \\
       since the explanatory variable is expected to be categorical.'
    )
  }

  x <- x |>
    select(any_of(c(response_name(x), explanatory_name(x))))

  stats::chisq.test(table(x), ...) |>
    broom::glance() |>
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)
}

#' Tidy chi-squared test statistic
#'
#'  @description
#'
#' A shortcut wrapper function to get the observed test statistic for a chisq
#' test. Uses [chisq.test()][stats::chisq.test()], which applies a continuity
#' correction. This function has been deprecated in favor of the more
#' general [observe()].
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @inheritParams specify
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
#' @family wrapper functions
#' @family functions for calculating observed statistics
#' @export
chisq_stat <- function(x, formula, response = NULL, explanatory = NULL, ...) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "chisq_stat()",
    with = "observe()"
  )

  # Parse response and explanatory variables
  response <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- standardize_variable_types(x)

  x <- parse_variables(
    x = x,
    formula = formula,
    response = response,
    explanatory = explanatory
  )

  if (!(class(response_variable(x)) %in% c("logical", "character", "factor"))) {
    cli_abort(
      'The response variable of `{response_name(x)}` is not appropriate \\
       since the response variable is expected to be categorical.'
    )
  }
  if (
    has_explanatory(x) &&
      !(class(explanatory_variable(x)) %in% c("logical", "character", "factor"))
  ) {
    cli_abort(
      'The explanatory variable of `{explanatory_name(x)}` is not appropriate \\
       since the response variable is expected to be categorical.'
    )
  }

  x <- x |>
    select(any_of(c(response_name(x), explanatory_name(x))))

  suppressWarnings(stats::chisq.test(table(x), ...)) |>
    broom::glance() |>
    dplyr::select(statistic) |>
    pull()
}

check_conf_level <- function(conf_level, call = caller_env()) {
  if (
    (!inherits(conf_level, "numeric")) | (conf_level < 0) | (conf_level > 1)
  ) {
    cli_abort(
      "The `conf_level` argument must be a number between 0 and 1.",
      call = call
    )
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
#' @inheritParams specify
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
#' @inheritParams t_test
#' @param success The level of `response` that will be considered a success, as
#'   a string. Only used when testing the null that a single
#'   proportion equals a given value, or that two proportions are equal;
#'   ignored otherwise.
#' @param correct A logical indicating whether Yates' continuity correction
#'   should be applied where possible. If `z = TRUE`, the `correct` argument will
#'   be overwritten as `FALSE`. Otherwise defaults to `correct = TRUE`.
#' @param z A logical value for whether to report the statistic as a standard
#'   normal deviate or a Pearson's chi-square statistic. \eqn{z^2}  is distributed
#'   chi-square with 1 degree of freedom, though note that the user will likely
#'   need to turn off Yates' continuity correction by setting `correct = FALSE`
#'   to see this connection.
#' @param ... Additional arguments for [prop.test()][stats::prop.test()].
#'
#' @details
#' When testing with an explanatory variable with more than two levels, the
#' `order` argument as used in the package is no longer well-defined. The function
#' will thus raise a warning and ignore the value if supplied a non-NULL `order`
#' argument.
#'
#' The columns present in the output depend on the output of both [prop.test()]
#' and [broom::glance.htest()]. See the latter's documentation for column
#' definitions; columns have been renamed with the following mapping:
#'
#' * `chisq_df` = `parameter`
#' * `p_value` = `p.value`
#' * `lower_ci` = `conf.low`
#' * `upper_ci` = `conf.high`
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
#' # report as a z-statistic rather than chi-square
#' # and specify the success level of the response
#' prop_test(gss,
#'           college ~ NULL,
#'           success = "degree",
#'           p = .2,
#'           z = TRUE)
#'
#' @family wrapper functions
#' @export
prop_test <- function(
  x,
  formula,
  response = NULL,
  explanatory = NULL,
  p = NULL,
  order = NULL,
  alternative = "two-sided",
  conf_int = TRUE,
  conf_level = 0.95,
  success = NULL,
  correct = NULL,
  z = FALSE,
  ...
) {
  # Parse response and explanatory variables
  response <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- standardize_variable_types(x)

  x <- parse_variables(
    x = x,
    formula = formula,
    response = response,
    explanatory = explanatory
  )

  correct <- if (z) {
    FALSE
  } else if (is.null(correct)) {
    TRUE
  } else {
    correct
  }

  if (!(class(response_variable(x)) %in% c("logical", "character", "factor"))) {
    cli_abort(
      'The response variable of `{response_name(x)}` is not appropriate \\
      since the response variable is expected to be categorical.'
    )
  }
  if (
    has_explanatory(x) &&
      !(class(explanatory_variable(x)) %in% c("logical", "character", "factor"))
  ) {
    cli_abort(
      'The explanatory variable of `{explanatory_name(x)}` is not appropriate \\
       since the explanatory variable is expected to be categorical.'
    )
  }
  # match with old "dot" syntax in t.test
  if (alternative %in% c("two-sided", "two_sided", "two sided", "two.sided")) {
    alternative <- "two.sided"
  }

  # process "success" arg
  lvls <- levels(factor(response_variable(x)))

  if (length(lvls) > 2) {
    cli_abort(
      "This test is not defined for response variables with more than 2 levels."
    )
  }

  if (!is.null(success)) {
    check_type(success, rlang::is_string)

    if (!(success %in% lvls)) {
      cli_abort('{success} is not a valid level of {response_name(x)}.')
    }

    lvls <- c(success, lvls[lvls != success])
  } else {
    success <- lvls[1]
  }

  # two sample
  if (has_explanatory(x)) {
    # make a summary table to supply to prop.test
    sum_table <- x |>
      select(explanatory_name(x), response_name(x)) |>
      table()

    length_exp_levels <- length(levels(explanatory_variable(x)))
    if (length_exp_levels == 2) {
      order <- check_order(x, order, in_calculate = FALSE, stat = NULL)
      # reorder according to the order and success arguments
      sum_table <- sum_table[order, lvls]
    } else if (length_exp_levels >= 3 && !is.null(order)) {
      cli_warn(c(
        "The `order` argument will be ignored as it is not well-defined \\
             for explanatory variables with more than 2 levels. ",
        i = "To silence this message, avoid passing the `order` argument."
      ))
      # reorder according to the success argument
      sum_table <- sum_table[, lvls]
    }

    prelim <- stats::prop.test(
      x = sum_table,
      alternative = alternative,
      conf.level = conf_level,
      p = p,
      correct = correct,
      ...
    )
  } else {
    # one sample
    response_tbl <- response_variable(x) |>
      factor() |>
      stats::relevel(success) |>
      table()

    if (is.null(p)) {
      cli_inform(
        "No `p` argument was hypothesized, so the test will \\
         assume a null hypothesis `p = .5`."
      )
    }

    prelim <- stats::prop.test(
      x = response_tbl,
      alternative = alternative,
      conf.level = conf_level,
      p = p,
      correct = correct,
      ...
    )
  }

  if (length(prelim$estimate) <= 2) {
    if (conf_int & is.null(p)) {
      results <- prelim |>
        broom::glance() |>
        dplyr::select(
          statistic,
          chisq_df = parameter,
          p_value = p.value,
          alternative,
          lower_ci = conf.low,
          upper_ci = conf.high
        )
    } else {
      results <- prelim |>
        broom::glance() |>
        dplyr::select(
          statistic,
          chisq_df = parameter,
          p_value = p.value,
          alternative
        )
    }
  } else {
    results <- prelim |>
      broom::glance() |>
      dplyr::select(statistic, chisq_df = parameter, p_value = p.value)
  }

  if (z) {
    results <- calculate_z(x, results, success, p, order)
  }

  results
}

calculate_z <- function(x, results, success, p, order) {
  exp <- if (has_explanatory(x)) {
    explanatory_expr(x)
  } else {
    NULL
  }

  form <- new_formula(response_expr(x), exp)

  stat <- x |>
    specify(formula = form, success = success) |>
    hypothesize(
      null = if (has_explanatory(x)) {
        "independence"
      } else {
        "point"
      },
      p = if (is.null(p) && !has_explanatory(x)) {
        .5
      } else {
        p
      }
    ) |>
    calculate(
      stat = "z",
      order = if (has_explanatory(x)) {
        order
      } else {
        NULL
      }
    ) |>
    dplyr::pull()

  results$statistic <- stat
  results$chisq_df <- NULL

  results
}
