#' Specify a null hypothesis
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param null the null hypothesis
#' @param ... arguments passed to downstream functions
#' @importFrom dplyr as.tbl
#' @export
#' @examples 
#' if (require(dplyr)) {
#' 
#' # One binary variable
#'   mtcars %>%
#'     select(am) %>%
#'     hypothesis(null = "p = 25") %>%
#'     generate(reps = 100) %>%
#'     calculate(stat = "prop")
#'     
#' # Permutation test
#'   mtcars %>%
#'     select(mpg, cyl) %>%
#'     hypothesis(null = "rho = 0") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "cor")
#' }
#' 
#' # Compare with
#' if (require(dplyr) && require(broom)) {
#'   cars <- mtcars %>%
#'     summarize(N = n(), num_manual = sum(am))
#'   with(cars, prop.test(num_manual, N, correct = FALSE)) %>%
#'     tidy()  
#' }

hypothesis <- function(x, null, ...) {
  attr(x, "null") <- null
  return(as.tbl(x))
}

#' Generate resamples
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param reps the number of resamples to generate
#' @param type currently either \code{bootstrap} or \code{permute}
#' @param ... currently ignored
#' @importFrom dplyr group_by
#' @export

generate <- function(x, reps = 1, type = "bootstrap", ...) {
  if (type == "bootstrap") {
    return(bootstrap(x, reps, ...))
  }
  if (type == "permute") {
    return(permute(x, reps, ...))
  }
  x
}

bootstrap <- function(x, reps = 1, ...) {
  rep_sample_n(x, size = nrow(x), replace = TRUE, reps = reps)
}

#' @importFrom dplyr bind_rows mutate_ group_by

permute <- function(x, reps = 1, ...) {
  replicate(reps, permute_once(x), simplify = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate_(replicate = ~rep(1:reps, each = nrow(x))) %>%
    dplyr::group_by(replicate)
}

permute_once <- function(x, ...) {
  dots <- list(...)
  # need to look for name of variable to permute...ugh
  # by default, use the first column
  y <- x[, 1]
  y_prime <- y[ sample.int(length(y)) ]
  x[, 1] <- y_prime
  return(x)
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

#' Calculate a p-value
#' @param x the output from \code{\link{calculate}}
#' @param ... currently ignored
#' @export

pvalue <- function(x, ...) {
  x
}

# stolen from oilabs::rep_sample_n()
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1) {
  n <- nrow(tbl)
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace), 
                        simplify = FALSE))
  rep_tbl <- cbind(replicate = rep(1:reps, rep(size, reps)), 
                   tbl[i, ])
  dplyr::group_by(rep_tbl, replicate)
}