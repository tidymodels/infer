#' Pipe
#'
#' Like \code{dplyr}, \code{infer} also uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs  Inference functions and the initial data frame
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
    cat(paste("Response: ", attr(x, "response"), "\n"),
              "Response Type: ", attr(x, "response_type"), "\n")  
    if ("explanatory" %in% attrs) {
      cat(paste("Explanatory: ", attr(x, "explanatory"), "\n"),
                "Explanatory Type: ", attr(x, "explanatory_type"), "\n")
    }
  }
  if ("null" %in% attrs) {
    cat(paste("Null Hypothesis: ", attr(x, "null"), "\n"))
  }
  NextMethod()
}