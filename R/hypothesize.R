#' Declare a null hypothesis
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param null the null hypothesis. Options include "independence" and "point".
#' @param ... arguments passed to downstream functions
#' @importFrom dplyr as.tbl
#' @export
#' @examples
#' # To see the output of hypothesize() function provided
#' # in the infer class
#' if(require(dplyr)){
#'   mtcars %>%
#'     mutate(am = factor(am)) %>%
#'     specify(response = am) %>%
#'     hypothesize(null = "point", p = c("0" = 0.25, "1" = 0.75))
#' }
#'
#' # hypothesize() assigns attributes to an infer class
#' if(require(dplyr)){
#'   mtcars %>%
#'     mutate(am = factor(am)) %>%
#'     specify(response = am) %>%
#'     hypothesize(null = "point", p = c("0" = 0.25, "1" = 0.75)) %>%
#'     class()
#' }
#'
#' # To view where hypothesize() falls in the infer package pipeline
#' if(require(dplyr)) {
#' # One binary variable
#'   mtcars %>%
#'     mutate(am = factor(am)) %>%
#'     specify(response = am) %>%
#'     hypothesize(null = "point", p = c("0" = 0.25, "1" = 0.75)) %>%
#'     generate(reps = 100, type = "simulate") %>%
#'     calculate(stat = "prop")
#' }
#'
#' if(require(dplyr)) {
#' # Permutation test
#'   mtcars %>%
#'     mutate(cyl = factor(cyl)) %>%
#'     specify(mpg ~ cyl) %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "F")
#' }
#'
#' # Compare with traditional "tidy" hypothesis test results
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

  # error: Didn't specify an explanatory variable with test of independence
  if(is.null(attr(x, "explanatory")) & null == "independence"){
    stop('specify() requires an explanatory variable for null = "independence"')
  }

  dots <- list(...)
  if (length(dots) > 0) {
    params <- parse_params(dots, x)
    attr(x, "params") <- params
  }

  return(as.tbl(x))
}

parse_params <- function(dots, x) {
  p_ind <- grep("p", names(dots))
  mu_ind <- grep("mu", names(dots))
  med_ind <- grep("med", names(dots))
  sig_ind <- grep("sigma", names(dots))

  # error: cannot specify more than one of props, means, medians, or sds
  if ( length(p_ind) + length(mu_ind) + length(med_ind) + length(sig_ind) != 1 ){
    stop("Parameter values should be only one of proportions, means, medians, or standard deviations.")
  }

  # add in 1 - p if it's missing
  # Outside if() is needed to ensure an error does not occur in referencing the
  # 0 index of dots
  if (length(p_ind)) {
    if (length(dots[[p_ind]]) == 1 &
        length(levels(x[[as.character(attr(x, "response"))]])) == 2) {
      missing_lev <- setdiff(levels(pull(x, !!attr(x, "response"))), attr(x, "success"))
      dots$p <- append(dots$p, 1 - dots$p)
      names(dots$p) <- c(attr(x, "success"), missing_lev)
    }
    else if (length(dots[[p_ind]]) != length(levels(x[[as.character(attr(x, "response"))]]))){
      stop(paste("The factor variable that you have specified has",
                 length(levels(x[[as.character(attr(x, "response"))]])),
                 "levels and you've only assigned null probabilities to",
                length(dots[[p_ind]]),
                "levels."))
    }
  }

  # if (sum(dots[[p_ind]]) != 1){
  #   dots[[p_ind]] <- dots[[p_ind]]/sum(dots[[p_ind]])
  #   warning("Proportions do not sum to 1, normalizing automatically.")
  # }

  return(unlist(dots))
}
