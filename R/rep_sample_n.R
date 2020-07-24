#' Perform repeated sampling
#'
#' @description
#'
#' Perform repeated sampling of samples of size n. Useful for creating sampling
#' distributions.
#'
#' @param tbl Data frame of population from which to sample.
#' @param size Sample size of each sample.
#' @param replace Should sampling be with replacement?
#' @param reps Number of samples of size n = `size` to take.
#' @param prob A vector of probability weights for obtaining the elements of the
#'   vector being sampled.
#'
#' @return A tibble of size `rep` times `size` rows corresponding to `rep`
#'   samples of size n = `size` from `tbl`.
#'   
#' @details The [dplyr::sample_n()] function from that `rep_sample_n()` function 
#' was originally written to supplement has been superseded 
#' by [dplyr::slice_sample()]. `rep_slice_sample()` provides a light wrapper
#' around `rep_sample_n()` that has a more similar interface to `slice_sample()`
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' suppressPackageStartupMessages(library(ggplot2))
#'
#' # A virtual population of N = 10,010, of which 3091 are hurricanes
#' population <- dplyr::storms %>%
#'   select(status)
#'
#' # Take samples of size n = 50 storms without replacement; do this 1000 times
#' samples <- population %>%
#'   rep_sample_n(size = 50, reps = 1000)
#' samples
#'
#' # Compute p_hats for all 1000 samples = proportion hurricanes
#' p_hats <- samples %>%
#'   group_by(replicate) %>%
#'   summarize(prop_hurricane = mean(status == "hurricane"))
#' p_hats
#'
#' # Plot sampling distribution
#' ggplot(p_hats, aes(x = prop_hurricane)) +
#'   geom_density() +
#'   labs(x = "p_hat", y = "Number of samples",
#'   title = "Sampling distribution of p_hat from 1000 samples of size 50")
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
rep_slice_sample <- function(tbl, n, replace = FALSE, reps = 1, weight_by = NULL) {
  rep_sample_n(tbl, n, replace, reps, weight_by)
}
