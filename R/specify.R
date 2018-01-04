#' Specify the response and explanatory variables
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param response the variable name in \code{x} that will serve as the response. This is alternative to using the \code{formula} argument.
#' @param explanatory the variable name in \code{x} that will serve as the explanatory variable
#' @param success the level of \code{response} that will be considered a success, as a string. Needed for inference on one proportion or a difference in proportions.
#' @return A tibble containing the response (and explanatory, if specified) variable data
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @importFrom dplyr mutate_if select one_of as_tibble
#' @importFrom methods hasArg
#' @export

specify <- function(x, formula, response = NULL, explanatory = NULL, success = NULL) {
  assertive::assert_is_data.frame(x)

  # Convert all character variables to be factor variables
  x <- dplyr::as_tibble(x) %>% mutate_if(is.character, as.factor)

  if (!methods::hasArg(formula) && !methods::hasArg(response)) {
    stop("Please specify the response variable.")
  }
  if (methods::hasArg(formula)) {
    if (!rlang::is_formula(formula)) {
      stop("The `formula` argument is not recognized as a formula.")
    }
  }

  attr(x, "response")    <- substitute(response)
  attr(x, "explanatory") <- substitute(explanatory)

  if (methods::hasArg(formula)) {
    attr(x, "response")    <- f_lhs(formula)
    attr(x, "explanatory") <- f_rhs(formula)
  }
  response_col <- rlang::eval_tidy(attr(x, "response"), x)

  if (!as.character(attr(x, "response")) %in% names(x)) {
    stop(paste0("The response variable `", attr(x, "response"), "` cannot be found in this dataframe."))
  }

  # if there's an explanatory var
  if (!(is.null(attr(x, "explanatory")) || as.character(attr(x, "explanatory")) == "1")) {
    if (!as.character(attr(x, "explanatory")) %in% names(x)) {
      stop(paste0("The explanatory variable `", attr(x, "explanatory"), "` cannot be found in this dataframe."))
    }
    if (identical(as.character(attr(x, "response")), as.character(attr(x, "explanatory")))) {
      stop("The response and explanatory variables must be different from one another.")
    }
    explanatory_col <- rlang::eval_tidy(attr(x, "explanatory"), x)
    if (is.character(explanatory_col)) {
      rlang::eval_tidy(attr(x, "explanatory"), x) <- as.factor(explanatory_col)
    }
  }

  attr(x, "success") <- success

  if (!is.null(success)) {
    if (!is.character(success)) {
      stop("`success` must be a string.")
    }
    if (!is.factor(response_col)) {
      stop("`success` should only be specified if the response is a categorical variable.")
    }
    if (!(success %in% levels(response_col))) {
      stop(paste0(success, " is not a valid level of ", attr(x, "response"), "."))
    }
  }

  x <- x %>%
    select(one_of(c(
      as.character((attr(x, "response"))),
      as.character(attr(x, "explanatory"))
    )))

  # add "infer" class
  class(x) <- append("infer", class(x))

  return(x)
}
