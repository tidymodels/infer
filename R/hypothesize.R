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
hypothesize <- function(x, null, p = NULL, mu = NULL, med = NULL, sigma = NULL) {

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
    independence =  {
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
          abort(paste0(
            'Testing one categorical variable requires `p` to be used as a ',
            'parameter.'
          ))
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
     abort(paste0("x must be a data.frame or tibble"), call = call)
  }

  if ((null == "independence") && !has_explanatory(x)) {
     abort(paste0(
      'Please `specify()` an explanatory and a response variable when ',
      'testing a null hypothesis of `"independence"`.'
    ), call = call)
  }

  if (null == "paired independence" && has_explanatory(x)) {
     abort(
        c(      paste0('Please `specify()` only a response variable when ',
                       'testing a null hypothesis of `"paired independence"`.'),
          "i" = paste0('The supplied response variable should be the ',
                       'pre-computed difference between paired observations.')),
       call = call
     )
  }
}

match_null_hypothesis <- function(null, call = caller_env()) {
  null_hypothesis_types <- c("point", "independence", "paired independence")

  if(length(null) != 1) {
     abort(paste0('You should specify exactly one type of null hypothesis.'),
           call = call)
  }

  i <- pmatch(null, null_hypothesis_types)

  if(is.na(i)) {
     abort(paste0('`null` should be either "point", "independence",',
                  ' or "paired independence".'),
           call = call)
  }

  null_hypothesis_types[i]
}

sanitize_hypothesis_params_independence <- function(dots) {
  if (length(dots) > 0) {
     warn(paste0(
      "Parameter values are not specified when testing that two variables are ",
      "independent."
    ))
  }

  NULL
}

sanitize_hypothesis_params_point <- function(dots, x, call = caller_env()) {
  if(length(dots) != 1) {
     abort(paste0("You must specify exactly one of `p`, `mu`, `med`, or `sigma`."),
           call = call)
  }

  if (!is.null(dots$p)) {
    dots$p <- sanitize_hypothesis_params_proportion(dots$p, x, call = call)
  }

  dots
}

sanitize_hypothesis_params_proportion <- function(p, x, call = caller_env()) {
  eps <- if (capabilities("long.double")) {sqrt(.Machine$double.eps)} else {0.01}

  if(anyNA(p)) {
     abort(paste0('`p` should not contain missing values.'),
           call = call)
  }

  if(any(p < 0 | p > 1)) {
     abort(paste0('`p` should only contain values between zero and one.'),
           call = call)
  }

  if(length(p) == 1) {
    if(!has_attr(x, "success")) {
       abort(paste0(
        "A point null regarding a proportion requires that `success` ",
        "be indicated in `specify()`."
      ), call = call)
    }

    p <- c(p, 1 - p)
    names(p) <- get_success_then_response_levels(x)
  } else {
    if (sum(p) < 1 - eps | sum(p) > 1 + eps) {
       abort(paste0(
        "Make sure the hypothesized values for the `p` parameters sum to 1. ",
        "Please try again."
      ), call = call)
    }
  }

  p
}

sanitize_hypothesis_params_paired_independence <- function(dots) {
   if (length(dots) > 0) {
      warn(
        "Parameter values are not specified when testing paired independence."
      )
   }

   NULL
}
