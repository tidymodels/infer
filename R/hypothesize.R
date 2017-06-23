#' Specify a null hypothesis
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param null the null hypothesis
#' @param ... arguments passed to downstream functions
#' @importFrom dplyr as.tbl
#' @export
#' @examples
#' if (require(dplyr)) {
#'
#' # One binary variable
#'   mtcars %>%
#'     select(am) %>%
#'     hypothesize(null = "p = 25") %>%
#'     generate(reps = 100) %>%
#'     calculate(stat = "prop")
#'
#' # Permutation test
#'   mtcars %>%
#'     select(mpg, cyl) %>%
#'     hypothesize(null = "rho = 0") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "cor")
#' }
#'
#' # Compare with
#' if (require(dplyr) && require(broom)) {
#'   cars <- mtcars %>%
#'     summarize(N = n(), num_manual = sum(am))
#'   with(cars, prop.test(num_manual, N, correct = FALSE)) %>%
#'     tidy()
#' }

hypothesize <- function(x, null, ...) {
  attr(x, "null") <- null
  return(as.tbl(x))
}
