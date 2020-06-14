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
#'
#' @importFrom dplyr pull
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @export
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1, prob = NULL) {
  n <- nrow(tbl)

  check_type(tbl, is.data.frame)
  check_type(size, is.numeric)
  check_type(replace, is.logical)
  check_type(reps, is.numeric)
  if (!is.null(prob)) {
    check_type(prob, is.numeric)
  }

  # assign non-uniform probabilities
  # there should be a better way!!
  # prob needs to be nrow(tbl) -- not just number of factor levels
  if (!is.null(prob)) {
    if (length(prob) != n) {
      stop_glue(
        "The argument `prob` must have length `nrow(tbl)` = {nrow(tbl)}"
      )
    }

    prob <- tibble::tibble(vals = levels(dplyr::pull(tbl, 1))) %>%
      dplyr::mutate(probs = prob) %>%
      dplyr::inner_join(tbl) %>%
      dplyr::select(probs) %>%
      dplyr::pull()
  }

  i <- unlist(replicate(
    reps,
    sample.int(n, size, replace = replace, prob = prob),
    simplify = FALSE
  ))
  rep_tbl <- cbind(
    replicate = rep(1:reps, rep(size, reps)),
    tbl[i, ]
  )
  rep_tbl <- tibble::as_tibble(rep_tbl)
  names(rep_tbl)[-1] <- names(tbl)
  dplyr::group_by(rep_tbl, replicate)
}
