#' Declare a null hypothesis
#'
#' @description
#'
#' Declare a null hypothesis about variables selected in [specify()].
#'
#' Learn more in `vignette("infer")`.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param null The null hypothesis. Options include `"independence"`,
#'   `"point"`, and `"paired independence"`.
#' \itemize{
#'   \item `independence`: Should be used with both a `response` and `explanatory`
#'   variable. Indicates that the values of the specified `response` variable
#'   are independent of the associated values in `explanatory`.
#'   \item `point`: Should be used with only a `response` variable. Indicates
#'   that a point estimate based on the values in `response` is associated
#'   with a parameter. Sometimes requires supplying one of `p`, `mu`, `med`, or
#'   `sigma`.
#'   \item `paired independence`: Should be used with only a `response` variable
#'   giving the pre-computed difference between paired observations. Indicates
#'   that the order of subtraction between paired values does not affect the
#'   resulting distribution.
#' }
#' @param p The true proportion of successes (a number between 0 and 1). To be used with point null hypotheses when the specified response
#' variable is categorical.
#' @param mu The true mean (any numerical value). To be used with point null
#' hypotheses when the specified response variable is continuous.
#' @param med The true median (any numerical value). To be used with point null
#' hypotheses when the specified response variable is continuous.
#' @param sigma The true standard deviation (any numerical value). To be used with
#' point null hypotheses.
#'
#' @return A tibble containing the response (and explanatory, if specified)
#'   variable data with parameter information stored as well.
#'
#' @examples
#' # hypothesize independence of two variables
#' gss %>%
#'  specify(college ~ partyid, success = "degree") %>%
#'  hypothesize(null = "independence")
#'
#' # hypothesize a mean number of hours worked per week of 40
#' gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40)
#'
#' # more in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @importFrom purrr compact
#' @family core functions
#' @export
hypothesize <- function(
  x,
  null,
  p = NULL,
  mu = NULL,
  med = NULL,
  sigma = NULL
) {
  # Check arguments
  if (missing(null)) {
    null <- NA
  }
  null <- match_null_hypothesis(null)
  hypothesize_checks(x, null)

  attr(x, "null") <- null
  attr(x, "hypothesized") <- TRUE

  dots <- compact(list(p = p, mu = mu, med = med, sigma = sigma))

  # Set parameters and determine appropriate generation type
  switch(
    null,
    independence = {
      params <- sanitize_hypothesis_params_independence(dots)
      attr(x, "type") <- "permute"
    },
    point = {
      params <- sanitize_hypothesis_params_point(dots, x)
      attr(x, "params") <- unlist(params)

      if (!is.null(params$p)) {
        attr(x, "type") <- "draw"
      } else {
        # Check one proportion test set up correctly
        if (is.factor(response_variable(x))) {
          cli_abort(
            'Testing one categorical variable requires `p` to be used as a \\
             parameter.'
          )
        }
        attr(x, "type") <- "bootstrap"
      }
    },
    `paired independence` = {
      params <- sanitize_hypothesis_params_paired_independence(dots)
      attr(x, "type") <- "permute"
    }
  )

  res <- append_infer_class(tibble::as_tibble(x))

  copy_attrs(to = res, from = x)
}

#' @rdname hypothesize
#' @export
hypothesise <- hypothesize

hypothesize_checks <- function(x, null, call = caller_env()) {
  if (!inherits(x, "data.frame")) {
    cli_abort("x must be a data.frame or tibble", call = call)
  }

  if ((null == "independence") && !has_explanatory(x)) {
    cli_abort(
      'Please {.fun specify} an explanatory and a response variable when \\
       testing a null hypothesis of `"independence"`.',
      call = call
    )
  }

  if (null == "paired independence" && has_explanatory(x)) {
    cli_abort(
      c(
        'Please {.fun specify} only a response variable when \\
           testing a null hypothesis of `"paired independence"`.',
        "i" = 'The supplied response variable should be the \\
                 pre-computed difference between paired observations.'
      ),
      call = call
    )
  }
}

match_null_hypothesis <- function(null, call = caller_env()) {
  null_hypothesis_types <- c("point", "independence", "paired independence")

  if (length(null) != 1) {
    cli_abort(
      'You should specify exactly one type of null hypothesis.',
      call = call
    )
  }

  i <- pmatch(null, null_hypothesis_types)

  if (is.na(i)) {
    cli_abort(
      '`null` should be either "point", "independence", or "paired independence".',
      call = call
    )
  }

  null_hypothesis_types[i]
}

sanitize_hypothesis_params_independence <- function(dots) {
  if (length(dots) > 0) {
    cli_warn(
      "Parameter values should not be specified when testing that two \\
       variables are independent."
    )
  }

  NULL
}

sanitize_hypothesis_params_point <- function(dots, x, call = caller_env()) {
  if (length(dots) != 1) {
    cli_abort(
      "You must specify exactly one of `p`, `mu`, `med`, or `sigma`.",
      call = call
    )
  }

  if (!is.null(dots$p)) {
    dots$p <- sanitize_hypothesis_params_proportion(dots$p, x, call = call)
  }

  dots
}

sanitize_hypothesis_params_proportion <- function(p, x, call = caller_env()) {
  eps <- if (capabilities("long.double")) {
    sqrt(.Machine$double.eps)
  } else {
    0.01
  }

  if (anyNA(p)) {
    cli_abort(
      '`p` should not contain missing values.',
      call = call
    )
  }

  if (any(p < 0 | p > 1)) {
    cli_abort(
      '`p` should only contain values between zero and one.',
      call = call
    )
  }

  if (length(p) == 1) {
    if (!has_attr(x, "success")) {
      cli_abort(
        "A point null regarding a proportion requires that `success` \\
          be indicated in `specify()`.",
        call = call
      )
    }

    p <- c(p, 1 - p)
    names(p) <- get_success_then_response_levels(x)
  } else {
    if (sum(p) < 1 - eps | sum(p) > 1 + eps) {
      cli_abort(
        "Make sure the hypothesized values for the `p` parameters sum to 1. \\
          Please try again.",
        call = call
      )
    }
  }

  p
}

sanitize_hypothesis_params_paired_independence <- function(dots) {
  if (length(dots) > 0) {
    cli_warn(
      "Parameter values should not be specified when testing paired independence."
    )
  }

  NULL
}
