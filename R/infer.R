#' infer: a grammar for statistical inference
#'
#' The objective of this package is to perform statistical inference using a
#' grammar that illustrates the underlying concepts and a format that coheres
#' with the tidyverse.
#'
#' @examples
#' # Example usage:
#' library(infer)
#'
#' @docType package
#' @name infer
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "prop", "stat", "xbar", "xtilde", "x", "..density..", "statistic", ".",
      "parameter", "p.value", "xmin", "xmax", "density", "denom", "diff_prop",
      "group_num", "n1", "n2", "num_suc", "p_hat", "total_suc", "explan",
      "probs", "conf.low", "conf.high"
    )
  )
}
