#' Specify response and explanatory variables
#'
#' @description
#'  
#' `specify()` is used to specify which columns in the supplied data frame are
#' the relevant response (and, if applicable, explanatory) variables. Note that 
#' character variables are converted to `factor`s.
#' 
#' Learn more in `vignette("infer")`.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right. Alternatively, a `response` and `explanatory`
#'   argument can be supplied.
#' @param response The variable name in `x` that will serve as the response.
#'   This is an alternative to using the `formula` argument.
#' @param explanatory The variable name in `x` that will serve as the
#'   explanatory variable. This is an alternative to using the formula argument.
#' @param success The level of `response` that will be considered a success, as
#'   a string. Needed for inference on one proportion, a difference in
#'   proportions, and corresponding z stats.
#'
#' @return A tibble containing the response (and explanatory, if specified)
#'   variable data.
#'
#' @examples
#' # specifying for a point estimate on one variable
#' gss %>%
#'    specify(response = age)
#' 
#' # specify a relationship between variables as a formula...
#' gss %>%
#'   specify(age ~ partyid)
#'   
#' # ...or with named arguments!
#' gss %>%
#'   specify(response = age, explanatory = partyid)
#'
#' # More in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @importFrom dplyr mutate_if select one_of
#' @importFrom methods hasArg
#' @export
specify <- function(x, formula, response = NULL,
                    explanatory = NULL, success = NULL) {
  check_type(x, is.data.frame)

  # Standardize variable types
  x <- tibble::as_tibble(x) %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.logical, as.factor) %>%
    mutate_if(is.integer, as.numeric)
  
  # Parse response and explanatory variables
  response <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- parse_variables(x = x, formula = formula, 
                       response = response, explanatory = explanatory)
  
  # Add attributes
  attr(x, "success") <- success
  attr(x, "generated") <- FALSE
  attr(x, "response_type") <- class(response_variable(x))
  if (attr_is_null(x, "explanatory")) {
    attr(x, "explanatory_type") <- NULL
  } else {
    attr(x, "explanatory_type") <- class(explanatory_variable(x))
  }
  
  check_success_arg(x, success)
  
  # Determine params for theoretical fit
  x <- set_params(x)

  # Select variables
  x <- x %>%
    select(one_of(c(response_name(x), explanatory_name(x))))

  is_complete <- stats::complete.cases(x)
  if (!all(is_complete)) {
    x <- dplyr::filter(x, is_complete)
    warning_glue("Removed {sum(!is_complete)} rows containing missing values.")
  }

  # Add "infer" class
  append_infer_class(x)
}

#' @importFrom rlang get_expr
parse_variables <- function(x, formula, response = NULL,
                            explanatory = NULL) {
  if (methods::hasArg(formula)) {
    tryCatch(
      rlang::is_formula(formula), 
      error = function(e) {
        stop_glue("The argument you passed in for the formula does not exist.
                  * Were you trying to pass in an unquoted column name?
                  * Did you forget to name one or more arguments?")
      }
    )
    if (!rlang::is_formula(formula)) {
      stop_glue("The first unnamed argument must be a formula.
                * You passed in '{get_type(formula)}'.
                * Did you forget to name one or more arguments?")
    }
  }
  
  attr(x, "response")    <- get_expr(response)
  attr(x, "explanatory") <- get_expr(explanatory)
  
  if (methods::hasArg(formula)) {
    attr(x, "response")    <- f_lhs(formula)
    attr(x, "explanatory") <- f_rhs(formula)
  }
  
  # Check response and explanatory variables to be appropriate for later use
  if (!has_response(x)) {
    stop_glue("Please supply a response variable that is not `NULL`.")
  }
  check_var_correct(x, "response")
  check_var_correct(x, "explanatory")
  
  # If there's an explanatory var
  check_vars_different(x)
  
  x
}

check_success_arg <- function(x, success) {
  response_col <- response_variable(x)
  
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
      stop_glue('{success} is not a valid level of {response_name(x)}.')
    }
    if (sum(table(response_col) > 0) > 2) {
      stop_glue(
        "`success` can only be used if the response has two levels. ",
        "`filter()` can reduce a variable to two levels."
      )
    }
  }
  
  if (attr(x, "response_type") == "bin" && is.null(success)) {
    if (attr(x, "explanatory_type") %in% c("num", "mult")) {
      stop_glue(
        'A level of the response variable `{response_name(x)}` needs to be ',
        'specified for the `success` argument in `specify()`.'
      )
    }
  }
}

check_var_correct <- function(x, var_name) {
  var <- attr(x, var_name)
  
  # Variable (if present) should be a symbolic column name
  if (!is.null(var)) {
    if (!rlang::is_symbolic(var)) {
      stop_glue(
        "The {var_name} should be a bare variable name (not a string in ",
        "quotation marks)."
      )
    }
    
    if (!(as.character(var) %in% names(x))) {
      stop_glue(
        'The {var_name} variable `{var}` cannot be found in this dataframe.'
      )
    }
  }
  
  TRUE
}

check_vars_different <- function(x) {
  if (has_response(x) && has_explanatory(x)) {
    if (identical(response_name(x), explanatory_name(x))) {
      stop_glue(
        "The response and explanatory variables must be different from one ",
        "another."
      )
    }
  }
  
  TRUE
}
