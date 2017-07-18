#' Specify the response and explanatory variables
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param response the variable name in \code{x} that will serve as the response. This is alternative to using the \code{formula} argument.
#' @param explanatory the variable name in \code{x} that will serve as the explanatory variable
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @importFrom tibble as_tibble
#' @export

specify <- function(x, formula, response = NULL, explanatory = NULL) {
  attr(x, "response")    <- substitute(response)
  attr(x, "explanatory") <- substitute(explanatory)

  if (hasArg(formula)) {
    attr(x, "response")    <- f_lhs(formula)
    attr(x, "explanatory") <- f_rhs(formula)
  }
  
  if (!all(
    as.character(
      c(attr(x, "response"),
        attr(x,"explanatory")
        )
    ) %in% names(x)
  )) stop("The columns you specified could not be found.")
  # TODO: coerce char to factor
  
  x <- as_tibble(x) %>%
    select(one_of(c(
      as.character((attr(x, "response"))),
      as.character(attr(x, "explanatory"))
    )))

  return(x)
}
