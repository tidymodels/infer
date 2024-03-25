#' Deprecated functions and objects
#'
#' These functions and objects should no longer be used. They will be removed
#' in a future release of infer.
#' @param x See the non-deprecated function.
#' @param level See the non-deprecated function.
#' @param type See the non-deprecated function.
#' @param point_estimate See the non-deprecated function.
#' @param obs_stat See the non-deprecated function.
#' @param direction See the non-deprecated function.
#' @seealso [get_p_value()], [get_confidence_interval()], [generate()]
#' @name deprecated
NULL


#' @rdname deprecated
#' @export
conf_int <- function(x, level = 0.95, type = "percentile",
  point_estimate = NULL) {
  lifecycle::deprecate_stop("0.4.0", "conf_int()", "get_confidence_interval()")
}


#' @rdname deprecated
#' @export
p_value <- function(x, obs_stat, direction) {
   lifecycle::deprecate_stop("0.4.0", "conf_int()", "get_p_value()")
}
