#' Perform repeated sampling
#'
#' @description
#'
#' These functions extend the functionality of [dplyr::sample_n()] and
#' [dplyr::slice_sample()] by allowing for repeated sampling of data.
#' This operation is especially helpful while creating sampling
#' distributions—see the examples below!
#'
#' @param tbl,.data Data frame of population from which to sample.
#' @param size,n,prop `size` and `n` refer to the sample size of each sample. 
#' The `size` argument to `rep_sample_n()` is required, while `n` defaults to 
#' 1 in `rep_slice_sample()`. `prop`, an argument to `rep_slice_sample()`, 
#' refers to the proportion of rows to sample in each sample, and is rounded 
#' down in the case that `prop * nrow(.data)` is not an integer. When using 
#' `rep_slice_sample()`, please only supply one of `n` or `prop`.
#' @param replace Should samples be taken with replacement?
#' @param reps Number of samples to take.
#' @param prob,weight_by A vector of sampling weights for each of the rows in
#' `.data`—must have length equal to `nrow(.data)`.
#'
#' @return A tibble of size `reps * n` rows corresponding to `reps`
#'   samples of size `n` from `.data`, grouped by `replicate`.
#'
#' @details The [dplyr::sample_n()] function (to which `rep_sample_n()` was
#' originally a supplement) has been superseded by [dplyr::slice_sample()].
#' `rep_sample_n()` now provides a light wrapper around `rep_slice_sample()`, 
#' which has a more similar interface to `slice_sample()`.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(tibble)
#'
#' # take 1000 samples of size n = 50, without replacement
#' slices <- gss %>%
#'   rep_slice_sample(n = 50, reps = 1000)
#'
#' slices
#'
#' # compute the proportion of respondents with a college
#' # degree in each replicate
#' p_hats <- slices %>%
#'   group_by(replicate) %>%
#'   summarize(prop_college = mean(college == "degree"))
#'
#' # plot sampling distribution
#' ggplot(p_hats, aes(x = prop_college)) +
#'   geom_density() +
#'   labs(
#'     x = "p_hat", y = "Number of samples",
#'     title = "Sampling distribution of p_hat"
#'   )
#'   
#' # sampling with probability weights. Note probabilities are automatically 
#' # renormalized to sum to 1
#' df <- tibble(
#'   id = 1:5,
#'   letter = factor(c("a", "b", "c", "d", "e"))
#' )
#' 
#' rep_slice_sample(df, n = 2, reps = 5, weight_by = c(.5, .4, .3, .2, .1))
#' @export
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1, prob = NULL) {
  rep_slice_sample(.data = tbl, n = size, replace = replace, reps = reps,
                   weight_by = prob, prop = NULL)
}

#' @rdname rep_sample_n
#' @export
rep_slice_sample <- function(.data, n = 1, replace = FALSE, weight_by = NULL,
                             reps = 1, prop = NULL) {
  check_type(.data, is.data.frame)
  check_type(replace, is.logical)
  check_type(reps, is.numeric)
  if (!is.null(weight_by)) {
    check_type(weight_by, is.numeric)
    if (length(weight_by) != nrow(.data)) {
      stop_glue(
        "The argument `weight_by` must have length `nrow(.data)` = {nrow(.data)}"
      )
    }
  }
  
  n <- process_slice_n(n, prop, missing(n), nrow(.data))
  
  1:reps %>%
    purrr::map_dfr(
      ~ .data %>%
        dplyr::slice_sample(n = n, weight_by = weight_by, replace = replace)
    ) %>%
    dplyr::mutate(
      replicate = rep(1:reps, each = n), 
      .before = dplyr::everything()
    ) %>%
    dplyr::group_by(replicate)
}

# an internal helper to check relevant arguments and determine the 
# appropriate sample size. note that `prop` doesn't get passed to
# rep_sample_n, so will not trigger any of these checks
process_slice_n <- function(n, prop, missing_n, n_pop) {
  if (!xor(missing_n, is.null(prop))) {
    stop_glue("Please supply one of the `n` or `prop` arguments.")
  }
  
  if (!missing_n) {
    check_type(n, is.numeric)
    return(n)
  }
    
  check_type(prop, is.numeric)
  if (prop > 1 || prop <= 0) {
    stop_glue("The `prop` argument must be a number in the interval (0, 1].")
  }
  return(floor(n_pop * prop))
}
