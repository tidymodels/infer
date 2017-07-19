#' Pipe graphics
#'
#' Like dplyr, infer also uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs  Inference functions
NULL

#' Print methods
#' 
#' @param x an object of class \code{infer}, i.e. output from \code{\link{specify}} 
#' or \code{\link{hypothesize}}
#' @param ... arguments passed to methods
#' @export
print.infer <- function(x, ...) {
  attrs <- names(attributes(x))
  if ("response" %in% attrs) {
    cat(paste("Response: ", attr(x, "response"), "\n"))  
    if ("explanatory" %in% attrs) {
      cat(paste("Explanatory: ", attr(x, "explanatory"), "\n"))
    }
  }
  if ("null" %in% attrs) {
    cat(paste("Null Hypothesis: ", attr(x, "null"), "\n"))
  }
  NextMethod()
}