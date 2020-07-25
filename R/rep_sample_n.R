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
#' @param size,n Sample size of each sample.
#' @param replace Should sampling be with replacement?
#' @param reps Number of samples of size n = `size` to take.
#' @param prob,weight_by A vector of sampling weights for each of the rows in `tbl`—must
#'   have length equal to `nrow(tbl)`.
#'
#' @return A tibble of size `rep * size` rows corresponding to `reps`
#'   samples of size `size` from `tbl`, grouped by `replicate`.
#'   
#' @details The [dplyr::sample_n()] function from that `rep_sample_n()` function 
#' was originally written to supplement has been superseded 
#' by [dplyr::slice_sample()]. `rep_slice_sample()` provides a light wrapper
#' around `rep_sample_n()` that has a more similar interface to `slice_sample()`.
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
#'   labs(x = "p_hat", y = "Number of samples",
#'   title = "Sampling distribution of p_hat")
#' @export
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1, prob = NULL) {
  check_type(tbl, is.data.frame)
  check_type(size, is.numeric)
  check_type(replace, is.logical)
  check_type(reps, is.numeric)
  if (!is.null(prob)) {
    check_type(prob, is.numeric)
  }

  1:reps %>%
    purrr::map_dfr(
      ~ tbl %>%
        dplyr::slice_sample(n = size, weight_by = prob, replace = replace)
    ) %>%
    dplyr::mutate(
      replicate = rep(1:reps, each = size), 
      .before = dplyr::everything()
    ) %>%
    dplyr::group_by(replicate)
}

#' @rdname rep_sample_n
#' @export
rep_slice_sample <- function(.data, n = 1, replace = FALSE, weight_by = NULL, reps = 1) {
  rep_sample_n(.data, n, replace, reps, weight_by)
}
