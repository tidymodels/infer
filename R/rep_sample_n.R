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
#' The `size` argument to `rep_sample_n()` is required (if `prop` is not 
#' supplied), while `n` defaults to 1 in `rep_slice_sample()`. `prop`, an 
#' argument to both functions, refers to the proportion of rows to sample in 
#' each sample, and is rounded down in the case that `prop * nrow(tbl)` is 
#' not an integer. Please only supply one of these arguments.
#' @param replace Should sampling be with replacement?
#' @param reps Number of samples of size n = `size` to take.
#' @param prob,weight_by A vector of sampling weights for each of the rows in
#' `tbl`—must have length equal to `nrow(tbl)`.
#'
#' @return A tibble of size `rep * size` rows corresponding to `reps`
#'   samples of size `size` from `tbl`, grouped by `replicate`.
#'
#' @details The [dplyr::sample_n()] function (to which `rep_sample_n()` was
#' originally a supplement) has been superseded by [dplyr::slice_sample()].
#' `rep_slice_sample()` provides a light wrapper around `rep_sample_n()` that
#' has a more similar interface to `slice_sample()`.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' # take 1000 samples of size n = 50, without replacement
#' slices <- gss %>%
#'   rep_sample_n(size = 50, reps = 1000)
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
#' library(tibble)
#' df <- tibble(
#'   id = 1:5,
#'   letter = factor(c("a", "b", "c", "d", "e"))
#' )
#' rep_sample_n(df, size = 2, reps = 5, prob = c(.5, .4, .3, .2, .1))
#' @export
rep_sample_n <- function(tbl, size = NULL, replace = FALSE, reps = 1, 
                         prob = NULL, prop = NULL) {
  rep_sample_internal(tbl, size, replace, reps, prob, NULL, prop,
                      in_slice = FALSE, missing_size = missing(size))
}

#' @rdname rep_sample_n
#' @export
rep_slice_sample <- function(.data, n = 1, replace = FALSE, weight_by = NULL,
                             reps = 1, prop = NULL) {
  rep_sample_internal(.data, NULL, replace, reps, weight_by, n, prop,
                      in_slice = TRUE, missing_size = missing(n))
}

rep_sample_internal <- function(tbl, size, replace = FALSE, reps = 1, 
                                prob = NULL, n, prop, in_slice, missing_size) {
  check_type(tbl, is.data.frame)
  check_type(replace, is.logical)
  check_type(reps, is.numeric)
  
  size <- process_slice_size(size, n, prop, in_slice, missing_size, nrow(tbl))
  
  if (!is.null(prob)) {
    check_type(prob, is.numeric)
    if (length(prob) != nrow(tbl)) {
      stop_glue(
        "The argument `prob` must have length `nrow(tbl)` = {nrow(tbl)}"
      )
    }
  }
  
  # Generate row indexes for every future replicate (this way it respects
  # possibility of  `replace = FALSE`)
  i <- unlist(replicate(
    reps,
    sample.int(nrow(tbl), size, replace = replace, prob = prob),
    simplify = FALSE
  ))
  
  tbl %>%
    dplyr::slice(i) %>%
    dplyr::mutate(replicate = rep(seq_len(reps), each = size)) %>%
    dplyr::select(replicate, dplyr::everything()) %>%
    tibble::as_tibble() %>%
    dplyr::group_by(replicate)
}

# an internal helper to check relevant arguments and determine the 
# appropriate sample size
process_slice_size <- function(size, n, prop, in_slice, missing_size, n_pop) {
  if (!xor(missing_size, is.null(prop))) {
    size_arg <- if (in_slice) {"n"} else {"size"}
    
    stop_glue("Please supply one of the `{size_arg}` or `prop` arguments.")
  }
  
  if (!missing_size) {
    if (in_slice) {
      check_type(n, is.numeric)
      
      return(n)
    } else {
      check_type(size, is.numeric)
      
      return(size)
    }
  }
  
  check_type(prop, is.numeric)
  
  if (prop > 1 || prop <= 0) {
    stop_glue("The `prop` argument must be a number in the interval (0, 1].")
  }
  
  return(floor(n_pop * prop))
}
