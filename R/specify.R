#' Specify the response and explanatory variables
#' @param x a data frame that can be coerced into a \code{\link[tibble]{tibble}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param response the variable name in \code{x} that will serve as the response. This is alternative to using the \code{formula} argument.
#' @param explanatory the variable name in \code{x} that will serve as the explanatory variable
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @importFrom dplyr mutate_if select one_of as_tibble
#' @importFrom methods hasArg
#' @export
#' @examples
#' # Response attribute set corresponding to
#' # response argument and specified variable selected
#' if(require(dplyr)){
#'   mtcars %>%
#'     mutate(am = factor(am)) %>%
#'     specify(response = am)
#' }
#' # Response and explanatory attributes set corresponding to
#' # response and explanatory arguments
#' if(require(dplyr)){
#'   mtcars %>%
#'     mutate(am = factor(am), vs = factor(vs)) %>%
#'     specify(response = am, explanatory = vs)
#' }
#' 
#' # Response and explanatory attributes set corresponding to
#' # formula argument expecting response ~ explanatory
#' if(require(dplyr)){
#'   mtcars %>%
#'     mutate(am = factor(am), vs = factor(vs)) %>%
#'     specify(formula = am ~ vs)
#' }

specify <- function(x, formula, response = NULL, explanatory = NULL) {
  
  # Convert all character variables to be factor variables instead
  x <- dplyr::as_tibble(x) %>% mutate_if(is.character, as.factor)
  
  attr(x, "response")    <- substitute(response)
  attr(x, "explanatory") <- substitute(explanatory)

  if (methods::hasArg(formula)) {
    attr(x, "response")    <- f_lhs(formula)
    attr(x, "explanatory") <- f_rhs(formula)
  }
  
  if (!all(
    as.character(
      c(attr(x, "response"),
        attr(x, "explanatory")
        )
    ) %in% names(x)
  )) stop("The columns you specified could not be found.")
  # TODO: coerce char to factor
  
  x <- as_tibble(x) %>%
    select(one_of(c(
      as.character((attr(x, "response"))),
      as.character(attr(x, "explanatory"))
    )))

  # add "infer" class
  class(x) <- append("infer", class(x))
  
  return(x)
}
