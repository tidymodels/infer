#' Declare a null hypothesis
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

  # error: null not found
  if (!(null %in% c("independence", "point"))) {
    stop("Choice of null is not supported.")
  }

  attr(x, "null") <- null

  dots <- list(...)
  if(length(dots) > 0) {
    params <- parse_params(dots, x)
    attr(x, "params") <- params
  }

  return(as.tbl(x))
}

parse_params <- function(dots, x) {
  p_ind <- grep("p", names(dots))
  ## ADD: check that props are between 0 and 1 and sum to 1 if there are > 1

  mu_ind <- grep("mu", names(dots))

  # error: cannot specify both props and means
  if (length(p_ind) * length(mu_ind) != 0) {
    stop("Parameter values should be either proportions or means but not both.")
  }

  # add in 1 - p if it's missing
  if (length(dots[[p_ind]]) == 1) {
    missing_lev <- setdiff(levels(pull(x, !! attr(x, "response"))), names(dots$p))
    dots$p <- append(dots$p, 1 - dots$p)
    names(dots$p)[2] <- missing_lev
  }

  return(unlist(dots))
}
