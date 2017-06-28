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
#'     hypothesize(null = "point", p = 0.25) %>%
#'     generate(reps = 100, type = "simulate") %>%
#'     calculate(stat = "prop")
#'
#' # Permutation test
#'   mtcars %>%
#'     mutate(cyl = factor(cyl)) %>%
#'     select(mpg, cyl) %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "F")
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
  classes <- sapply(x, class)

  # error: x is not a dataframe
  if (!sum(class(x) %in% c("data.frame", "tbl", "tbl_df", "grouped_df"))) {
    stop("x must be a data.frame or tibble")
  }

  # error: too many columns
  if (ncol(x) > 2) {
    stop("Too many columns. Use select() to narrow your data.frame.")
  }

  # error: null not found
  if (!(null %in% c("independence", "point"))) {
    stop("Choice of null is not supported.")
  }

  # error: independence requires factors
  if (null == "independence" & !("factor" %in% classes)) {
    stop("A null of independence requires at least one factor variable.")
  }

  attr(x, "null") <- null

  dots <- list(...)
  params <- parse_params(dots)

  # REVISIT after decision is made on vector vs list
  # error: number of parameters exceeds number of factor levels
#  if (length(params) != length(levels(x[,1]))) {
#    stop("The number of parameters must match the number of factor levels.")
#  }

  attr(x, "params") <- params

  return(as.tbl(x))
}

parse_params <- function(x) {
  ## find props
  p_ind <- grep("p\\d+", names(x))
  ## check that props are between 0 and 1 and sum to 1 if there are > 1

  # means
  ## find means
  mu_ind <- grep("mu\\d+", names(x))

  # error: cannot specify both props and means
  if (length(p_ind) * length(mu_ind) != 0) {
    stop("Parameter values should be either proportions or means but not both.")
  }

  return(x[c(p_ind, mu_ind)])
}
