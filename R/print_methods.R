#' Print methods
#'
#' @param x an object of class \code{infer}, i.e. output from \code{\link{specify}}
#' or \code{\link{hypothesize}}
#' @param ... arguments passed to methods
#' @export
print.infer <- function(x, ...) {
  attrs <- names(attributes(x))
  if ("response" %in% attrs) {
    cat(paste0("Response: ", attr(x, "response"), " (", attr(x, "response_type"), ")", "\n"))
    if ("explanatory" %in% attrs) {
      cat(paste0("Explanatory: ", attr(x, "explanatory"), " (", attr(x, "explanatory_type"), ")", "\n"))
    }
  }
  if ("null" %in% attrs) {
    cat(paste("Null Hypothesis: ", attr(x, "null"), "\n"))
  }
  NextMethod()
}
