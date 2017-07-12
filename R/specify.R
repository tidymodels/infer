#' Specify the response and explanatory variables
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param response the variable name in \code{x} that will serve as the response. This is alternative to using the \code{formula} argument.
#' @param explanatory the variable name in \code{x} that will serve as the explanatory variable
#' @importFrom rlang f_lhs
#' @importFrom rlang f_lhs
#' @export

specify <- function(x, formula, response = NULL, explanatory = NULL) {
  attr(x, "response")    <- substitute(response)
  attr(x, "explanatory") <- substitute(explanatory)

  if (hasArg(formula)) {
    attr(x, "response")    <- f_lhs(formula)
    attr(x, "explanatory") <- f_rhs(formula)
  }

  # TODO: coerce char to factor

  return(x)
}
