#' Specify the response and explanatory variables
#'
#' `specify()` also converts character variables chosen to be `factor`s.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right.
#' @param response The variable name in `x` that will serve as the response.
#'   This is alternative to using the `formula` argument.
#' @param explanatory The variable name in `x` that will serve as the
#'   explanatory variable.
#' @param success The level of `response` that will be considered a success, as
#'   a string. Needed for inference on one proportion, a difference in
#'   proportions, and corresponding z stats.
#'
#' @return A tibble containing the response (and explanatory, if specified)
#'   variable data.
#'
#' @examples
#' # Permutation test similar to ANOVA
#' mtcars %>%
#'   dplyr::mutate(cyl = factor(cyl)) %>%
#'   specify(mpg ~ cyl) %>%
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 100, type = "permute") %>%
#'   calculate(stat = "F")
#'
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @importFrom dplyr mutate_if select one_of
#' @importFrom methods hasArg
#' @export
specify <- function(x, formula, response = NULL,
                    explanatory = NULL, success = NULL) {
  check_type(x, is.data.frame)

  # Convert all character and logical variables to be factor variables
  x <- tibble::as_tibble(x) %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.logical, as.factor)

  if (!methods::hasArg(formula) && !methods::hasArg(response)) {
    stop_glue("Please give the `response` variable.")
  }
  if (methods::hasArg(formula)) {
    if (!rlang::is_formula(formula)) {
      stop_glue("The `formula` argument is not recognized as a formula.")
    }
  }

  attr(x, "response")    <- substitute(response)
  attr(x, "explanatory") <- substitute(explanatory)

  if (methods::hasArg(formula)) {
    attr(x, "response")    <- f_lhs(formula)
    attr(x, "explanatory") <- f_rhs(formula)
  }

  if (is.null(attr(x, "response"))) {
    stop_glue("Supply not `NULL` response variable.")
  }

  if (!(as.character(attr(x, "response")) %in% names(x))) {
    stop_glue(
      'The response variable `{attr(x, "response")}` cannot be found in this ',
      'dataframe.'
    )
  }

  response_col <- rlang::eval_tidy(attr(x, "response"), x)

  # if there's an explanatory var
  if (has_explanatory(x)) {
    if (!as.character(attr(x, "explanatory")) %in% names(x)) {
      stop_glue(
        'The explanatory variable `{attr(x, "explanatory")}` cannot be found ',
        'in this dataframe.'
      )
    }
    if (
      identical(
        as.character(attr(x, "response")), as.character(attr(x, "explanatory"))
      )
    ) {
      stop_glue(
        "The response and explanatory variables must be different from one ",
        "another."
      )
    }
    explanatory_col <- rlang::eval_tidy(attr(x, "explanatory"), x)
    if (is.character(explanatory_col)) {
      explanatory_col <- as.factor(explanatory_col)
    }
  }

  attr(x, "success") <- success

  if (!is.null(success)) {
    if (!is.character(success)) {
      stop_glue("`success` must be a string.")
    }
    if (!is.factor(response_col)) {
      stop_glue(
        "`success` should only be specified if the response is a categorical ",
        "variable."
      )
    }
    if (!(success %in% levels(response_col))) {
      stop_glue('{success} is not a valid level of {attr(x, "response")}.')
    }
    if (sum(table(response_col) > 0) > 2) {
      stop_glue(
        "`success` can only be used if the response has two levels. ",
        "`filter()` can reduce a variable to two levels."
      )
    }
  }

  x <- x %>%
    select(one_of(c(
      as.character((attr(x, "response"))), as.character(attr(x, "explanatory"))
    )))

  is_complete <- stats::complete.cases(x)
  if (!all(is_complete)) {
    x <- dplyr::filter(x, is_complete)
    warning_glue("Removed {sum(!is_complete)} rows containing missing values.")
  }

  # To help determine theoretical distribution to plot
  attr(x, "response_type") <- class(response_variable(x))

  if (is.null(attr(x, "explanatory"))) {
    attr(x, "explanatory_type") <- NULL
  } else {
    attr(x, "explanatory_type") <- class(explanatory_variable(x))
  }

  if (
    (attr(x, "response_type") == "factor") && is.null(success) &&
    (length(levels(response_variable(x))) == 2) &&
    (
      is.null(attr(x, "explanatory_type")) ||
      (
        !is.null(attr(x, "explanatory_type")) &&
        (length(levels(explanatory_variable(x))) == 2)
      )
    )
  ) {
    stop_glue(
      'A level of the response variable `{attr(x, "response")}` needs to be ',
      'specified for the `success` argument in `specify()`.'
    )
  }

  # Determine appropriate parameters for theoretical distribution fit
  x <- set_params(x)

  # add "infer" class
  class(x) <- append("infer", class(x))

  x
}
