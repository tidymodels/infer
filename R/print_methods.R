#' Print methods
#'
#' @param x An object of class `infer`, i.e. output from [specify()] or
#'   [hypothesize()], or of class `infer_layer`, i.e. output from
#'   [shade_p_value()] or [shade_confidence_interval()].
#' @param ... Arguments passed to methods.
#' @importFrom glue glue_collapse
#'
#' @rdname print.infer
#' @export
print.infer <- function(x, ...) {
  attrs <- names(attributes(x))
  header <- character(3)
  if ("response" %in% attrs) {
    header[1] <- glue_null(
      'Response: {response_name(x)} ({attr(x, "response_type")})'
    )
    if ("explanatory" %in% attrs) {
      header[2] <- glue_null(
        'Explanatory: {paste0(paste0(explanatory_name(x), " (",
        attr(x, "explanatory_type"), ")"), collapse = ", ")}'
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

#' @rdname print.infer
#' @export
print.infer_layer <- function(x, ...) {
  cat(x)
}

#' @rdname print.infer
#' @export
print.infer_dist <- function(x, ...) {
  cat(x)
}
