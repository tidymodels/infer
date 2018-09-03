#' Deprecated functions
#'
#' These functions should no longer be used. They will be removed in a
#' future release of \code{infer}.
#' @param x See the non-deprecated function.
#' @param level See the non-deprecated function.
#' @param type See the non-deprecated function.
#' @param point_estimate See the non-deprecated function.
#' @param obs_stat See the non-deprecated function.
#' @param direction See the non-deprecated function.
#' @seealso \code{\link{get_p_value}}, \code{\link{get_confidence_interval}}
#' @name deprecated
NULL


#' @rdname deprecated
#' @export
conf_int <- function(x, level = 0.95, type = "percentile",
  point_estimate = NULL) {
  .Deprecated("get_confidence_interval")
  get_confidence_interval(
    x, level = level, type = type, point_estimate = point_estimate
  )
}


#' @rdname deprecated
#' @export
p_value <- function(x, obs_stat, direction) {
  .Deprecated("get_p_value")
  get_p_value(x = x, obs_stat = obs_stat, direction = direction)
}

