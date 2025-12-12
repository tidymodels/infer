#' Print methods
#'
#' @param x An object of class `infer`, i.e. output from [specify()] or
#'   [hypothesize()], or of class `infer_layer`, i.e. output from
#'   [shade_p_value()] or [shade_confidence_interval()].
#' @param ... Arguments passed to methods.
#' @importFrom glue glue_collapse glue
#'
#' @rdname print.infer
#' @export
print.infer <- function(x, ...) {
  attrs <- names(attributes(x))
  header <- character(3)
  if ("response" %in% attrs) {
    header[1] <- glue(
      'Response: {response_name(x)} ({attr(x, "response_type")})',
      .null = "NULL"
    )
    if ("explanatory" %in% attrs) {
      header[2] <- glue(
        'Explanatory: {paste0(paste0(explanatory_name(x), " (",
        attr(x, "explanatory_type"), ")"), collapse = ", ")}',
        .null = "NULL"
      )
    }
  }
  if ("null" %in% attrs) {
    header[3] <- glue('Null Hypothesis: {attr(x, "null")}', .null = "NULL")
  }

  cat(glue::glue_collapse(
    header[header != ""],
    width = cli::console_width(),
    sep = "\n"
  ))
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
