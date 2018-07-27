#' Print methods
#'
#' @param x an object of class \code{infer}, i.e. output from \code{\link{specify}}
#' or \code{\link{hypothesize}}
#' @param ... arguments passed to methods
#' @export
print.infer <- function(x, ...) {
  attrs <- names(attributes(x))
  header <- character(3)
  if ("response" %in% attrs) {
    header[1] <- glue_null(
      'Response: {attr(x, "response")} ({attr(x, "response_type")})'
    )
    if ("explanatory" %in% attrs) {
      header[2] <- glue_null(
        'Explanatory: {attr(x, "explanatory")} ({attr(x, "explanatory_type")})'
      )
    }
  }
  if ("null" %in% attrs) {
    header[3] <- glue_null('Null Hypothesis: {attr(x, "null")}')
  }
  
  cat(glue::glue_collapse(header[header != ""], sep = "\n"))
  cat("\n")
  
  NextMethod()
}
