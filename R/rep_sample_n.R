#' Perform repeated sampling
#' 
#' Perform repeated sampling of samples of size n. Useful for creating
#' sampling distributions
#' 
#' @param tbl data frame of population from which to sample
#' @param size sample size of each sample
#' @param replace should sampling be with replacement?
#' @param reps number of samples of size n = \code{size} to take
#' @param prob a vector of probability weights for obtaining the elements of 
#' the vector being sampled.
#' @return A tibble of size \code{rep} times \code{size} rows corresponding to 
#' \code{rep} samples of size n = \code{size} from \code{tbl}.
#' @importFrom dplyr data_frame
#' @importFrom dplyr pull
#' @importFrom dplyr inner_join
#' @importFrom dplyr as_tibble
#' @importFrom dplyr group_by
#' 
#' @export
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' suppressPackageStartupMessages(library(ggplot2))
#' 
#' # Create a virtual population of N = 2400 balls, of which 900 are red and the
#' # rest are white
#' N <- 2400
#' population <- data_frame(
#'   ball_ID = 1:N,
#'   color = c(rep("red", 900), rep("white", N - 900))
#' )
#' population
#' 
#' # Take samples of size n = 50 balls without replacement; do this 1000 times
#' samples <- population %>%
#'   rep_sample_n(size = 50, reps = 1000)
#' samples
#'
#' # Compute p_hats for all 1000 samples = proportion red
#' p_hats <- samples %>% 
#'   group_by(replicate) %>% 
#'   summarize(prop_red = mean(color == "red"))
#' p_hats
#' 
#' # Plot sampling distribution
#' ggplot(p_hats, aes(x = prop_red)) + 
#'   geom_density() + 
#'   labs(x = "p_hat", y = "Number of samples", 
#'   title = "Sampling distribution of p_hat from 1000 samples of size 50")
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1, prob = NULL) {
  n <- nrow(tbl)
  
  assertive::assert_is_data.frame(tbl)
  assertive::assert_is_numeric(size)
  assertive::assert_is_logical(replace)
  assertive::assert_is_numeric(reps)
  if(!is.null(prob))
    assertive::assert_is_numeric(prob)
  
  # assign non-uniform probabilities
  # there should be a better way!!
  # prob needs to be nrow(tbl) -- not just number of factor levels
  if (!is.null(prob)) {
    if (length(prob) != n) 
      stop(paste("The argument `prob` must have length `nrow(tbl)` = ", 
                 nrow(tbl)))
    df_lkup <- dplyr::data_frame(vals = levels(dplyr::pull(tbl, 1)))
    names(df_lkup) <- names(tbl)
    df_lkup$probs <- prob
    tbl_wgt <- dplyr::inner_join(tbl, df_lkup)
    prob <- tbl_wgt$probs
  }
  
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace, 
                                         prob = prob),
                        simplify = FALSE))
  rep_tbl <- cbind(replicate = rep(1:reps, rep(size, reps)),
                   tbl[i, ])
  rep_tbl <- dplyr::as_tibble(rep_tbl)
  names(rep_tbl)[-1] <- names(tbl)
  dplyr::group_by(rep_tbl, replicate)
}

