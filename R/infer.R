#' infer: a grammar for statistical inference
#'
#' The objective of this package is to perform statistical inference using a
#' grammar that illustrates the underlying concepts and a format that coheres
#' with the tidyverse.
#'
#' For an overview of how to use the core functionality, see `vignette("infer")`
#'
#'
#' @docType package
#' @name infer
"_PACKAGE"

#' @importFrom cli cli_abort cli_warn cli_inform qty no

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "prop",
      "stat",
      "value",
      "x",
      "y",
      "..density..",
      "statistic",
      ".",
      "parameter",
      "p.value",
      "xmin",
      "x_min",
      "xmax",
      "x_max",
      "density",
      "denom",
      "diff_prop",
      "group_num",
      "n1",
      "n2",
      "num_suc",
      "p_hat",
      "total_suc",
      "explan",
      "probs",
      "conf.low",
      "conf.high",
      "prop_1",
      "prop_2",
      "data",
      "setNames",
      "resp",
      "capture.output",
      "stats",
      "estimate",
      "any_of",
      "model",
      "term",
      "where",
      "hypothesis"
    )
  )
}
