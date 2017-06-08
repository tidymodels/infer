#' Specify a null hypothesis
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param null the null hypothesis
#' @param ... arguments passed to downstream functions
#' @importFrom dplyr as.tbl
#' @export
#' @examples 
#' if (require(dplyr)) {
#'   mtcars %>%
#'     select(am) %>%
#'     hypothesis(null = "p = 25") %>%
#'     generate(reps = 100) %>%
#'     calculate(stat = "prop")
#' }

hypothesis <- function(x, null, ...) {
  attr(x, "null") <- null
  return(as.tbl(x))
}

#' Generate resamples
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param reps the number of resamples to generate
#' @param ... currently ignored
#' @importFrom dplyr group_by
#' @export

generate <- function(x, reps = 1, ...) {
  # stolen from oilabs::rep_sample_n()
  n <- nrow(x)
  size <- n
  replace <- TRUE
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace), 
                        simplify = FALSE))
  rep_tbl <- cbind(replicate = rep(1:reps, rep(size, reps)), x[i, ])
  return(dplyr::group_by(rep_tbl, replicate))
}

#' Calculate summary statistics
#' @param x the output from \code{\link{hypothesis}} or \code{\link{generate}}
#' @param ... currently ignored
#' @importFrom dplyr %>% group_by summarize_
#' @importFrom lazyeval interp
#' @export

calculate <- function(x, ...) {
  col <- setdiff(names(x), "replicate")
  x %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarize_(N = ~n(), 
                      mean = lazyeval::interp(~mean(var), var = as.name(col)))
}