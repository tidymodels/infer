#' Specify a null hypothesis
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param null the null hypothesis. Options include "independence" and "point".
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

hypothesize <- function(x, null = c("independence", "point"), ...) {
  attr(x, "null") <- null

  # functions to check if null makes sense given classes of variables

  dots <- list(...)
  params <- parse_params(dots)
  # functions to check if parameters make sense given classes of variables
  attr(x, "params") <- params

  return(as.tbl(x))
}

parse_params <- function(x) {
  # proportions
  ## find props
  prop_ind <- grep("p\\d+", names(x))
  ## check that props are between 0 and 1 and sum to 1 if there are > 1

  # means
  ## find props
  mean_ind <- grep("mu\\d+", names(x))

  return(x[c(prop_ind, mean_ind)])
}
