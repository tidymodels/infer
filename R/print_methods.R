#' Print methods
#'
#' @param x An object of class `infer`, i.e. output from [specify()] or
#'   [hypothesize()].
#' @param ... Arguments passed to methods.
#' @importFrom glue glue_collapse
#'
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
