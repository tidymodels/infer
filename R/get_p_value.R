#' Compute p-value
#'
#' @description
#'
#' Compute a p-value from a null distribution and observed statistic.
#'
#' Learn more in `vignette("infer")`.
#'
#' @param x A null distribution. For simulation-based inference, a data frame
#'   containing a distribution of [calculate()]d statistics
#'   or [`fit()`][fit.infer()]ted coefficient estimates. This object should
#'   have been passed to [generate()] before being supplied or
#'   [calculate()] to [`fit()`][fit.infer()]. For theory-based inference,
#'   the output of [assume()].
#' @param obs_stat A data frame containing the observed statistic (in a
#'   [calculate()]-based workflow) or observed fit (in a
#'   [`fit()`][fit.infer()]-based workflow). This object is likely the output
#'   of [calculate()] or [`fit()`][fit.infer()] and need not
#'   to have been passed to [generate()].
#' @param direction A character string. Options are `"less"`, `"greater"`, or
#'   `"two-sided"`. Can also use `"left"`, `"right"`, `"both"`,
#'   `"two_sided"`, or `"two sided"`, `"two.sided"`.
#'
#' @return A [tibble][tibble::tibble] containing the following columns:
#'
#' \itemize{
#'   \item `term`: The explanatory variable (or intercept) in question. Only
#'     supplied if the input had been previously passed to [`fit()`][fit.infer()].
#'   \item `p_value`: A value in \[0, 1\] giving the probability that a
#'     statistic/coefficient as or more extreme than the observed
#'     statistic/coefficient would occur if the null hypothesis were true.
#' }
#'
#'
#' @section Aliases:
#' `get_pvalue()` is an alias of `get_p_value()`.
#' `p_value` is a deprecated alias of `get_p_value()`.
#'
#' @section Zero p-value:
#' Though a true p-value of 0 is impossible, `get_p_value()` may return 0 in
#' some cases. This is due to the simulation-based nature of the \{infer\}
#' package; the output of this function is an approximation based on
#' the number of `reps` chosen in the `generate()` step. When the observed
#' statistic is very unlikely given the null hypothesis, and only a small
#' number of `reps` have been generated to form a null distribution,
#' it is possible that the observed statistic will be more extreme than
#' every test statistic generated to form the null distribution, resulting
#' in an approximate p-value of 0. In this case, the true p-value is a small
#' value likely less than `3/reps` (based on a poisson approximation).
#'
#' In the case that a p-value of zero is reported, a warning message will be
#' raised to caution the user against reporting a p-value exactly equal to 0.
#'
#'
#' @examples
#'
#' # using a simulation-based null distribution ------------------------------
#'
#' # find the point estimate---mean number of hours worked per week
#' point_estimate <- gss %>%
#'   specify(response = hours) %>%
#'   calculate(stat = "mean")
#'
#' # starting with the gss dataset
#' gss %>%
#'   # ...we're interested in the number of hours worked per week
#'   specify(response = hours) %>%
#'   # hypothesizing that the mean is 40
#'   hypothesize(null = "point", mu = 40) %>%
#'   # generating data points for a null distribution
#'   generate(reps = 1000, type = "bootstrap") %>%
#'   # finding the null distribution
#'   calculate(stat = "mean") %>%
#    # calculate the p-value for the point estimate
#'   get_p_value(obs_stat = point_estimate, direction = "two-sided")
#'
#' # using a theoretical null distribution -----------------------------------
#'
#' # calculate the observed statistic
#' obs_stat <- gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40) %>%
#'   calculate(stat = "t")
#'
#' # define a null distribution
#' null_dist <- gss %>%
#'   specify(response = hours) %>%
#'   assume("t")
#'
#' # calculate a p-value
#' get_p_value(null_dist, obs_stat, direction = "both")
#'
#' # using a model fitting workflow -----------------------------------------
#'
#' # fit a linear model predicting number of hours worked per
#' # week using respondent age and degree status.
#' observed_fit <- gss %>%
#'   specify(hours ~ age + college) %>%
#'   fit()
#'
#' observed_fit
#'
#' # fit 100 models to resamples of the gss dataset, where the response
#' # `hours` is permuted in each. note that this code is the same as
#' # the above except for the addition of the `generate` step.
#' null_fits <- gss %>%
#'   specify(hours ~ age + college) %>%
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 100, type = "permute") %>%
#'   fit()
#'
#' null_fits
#'
#' get_p_value(null_fits, obs_stat = observed_fit, direction = "two-sided")
#'
#' # more in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @name get_p_value
#' @export
get_p_value <- function(x, obs_stat, direction) {
  UseMethod("get_p_value", x)
}

#' @rdname get_p_value
#' @family auxillary functions
#' @method get_p_value default
#' @export
get_p_value.default <- function(x, obs_stat, direction) {
  check_type(x, is.data.frame)
  if (!is_generated(x) & is_hypothesized(x)) {
    cli_abort(c(
      "Theoretical p-values are not yet supported. ",
      i = "`x` should be the result of calling {.fun generate}."
    ))
  }
  check_for_nan(x, "get_p_value")
  check_direction(direction)

  if (is_fitted(x)) {
    # check that x and obs stat reference the same variables
    check_mlr_x_and_obs_stat(
      x,
      obs_stat,
      "get_p_value",
      "obs_stat"
    )

    # split up x and obs_stat by term
    term_data <- x %>%
      dplyr::ungroup() %>%
      dplyr::group_by(term) %>%
      dplyr::group_split() %>%
      purrr::map(copy_attrs, x)

    term_obs_stats <- obs_stat %>%
      dplyr::ungroup() %>%
      dplyr::group_by(term) %>%
      dplyr::group_split()

    # calculate the p value for each term and then add the term column back in
    purrr::map2_dfr(
      term_data,
      purrr::map(term_obs_stats, purrr::pluck, "estimate"),
      simulation_based_p_value,
      direction = direction
    ) %>%
      dplyr::mutate(
        term = purrr::map_chr(term_obs_stats, purrr::pluck, "term"),
        .before = dplyr::everything()
      )
  } else {
    simulation_based_p_value(x = x, obs_stat = obs_stat, direction = direction)
  }
}

#' @rdname get_p_value
#' @export
get_pvalue <- get_p_value

#' @rdname get_p_value
#' @method get_p_value infer_dist
#' @export
get_p_value.infer_dist <- function(x, obs_stat, direction) {
  # check the null hypotheses attached to x and obs_stat
  check_hypotheses_align(x, obs_stat)

  # parse the distribution function
  dist_fn <- paste0("p", attr(x, "distribution"))

  # translate the direction argument
  dir <- norm_direction(direction)

  lower_tail <- switch(
    dir,
    `left` = TRUE,
    `right` = FALSE,
    `both` = TRUE
  )

  # supply everything to the base R distribution function
  res <- do.call(
    dist_fn,
    c(
      list(q = as.numeric(obs_stat), lower.tail = lower_tail),
      process_df(attr(x, "df"))
    )
  )

  if (dir == "both") {
    res <- min(res, 1 - res) * 2
  }

  tibble::tibble(p_value = res)
}

simulation_based_p_value <- function(
  x,
  obs_stat,
  direction,
  call = caller_env()
) {
  check_x_vs_obs_stat(x, obs_stat, call = call)
  obs_stat <- check_obs_stat(obs_stat)

  # x[[ncol(x)]] pulls out the stat or estimate column
  if (direction %in% c("less", "left")) {
    pval <- left_p_value(x[[ncol(x)]], obs_stat)
  } else if (direction %in% c("greater", "right")) {
    pval <- right_p_value(x[[ncol(x)]], obs_stat)
  } else {
    pval <- two_sided_p_value(x[[ncol(x)]], obs_stat)
  }

  if (abs(pval) < 1e-16) {
    cli_warn(c(
      "Please be cautious in reporting a p-value of 0. This result is an \\
       approximation based on the number of `reps` chosen in the {.fun generate} step.",
      i = "See {.help [{.fun get_p_value}](infer::get_p_value)} for more information."
    ))
  }

  tibble::tibble(p_value = pval)
}

left_p_value <- function(vec, obs_stat) {
  mean(vec <= obs_stat)
}

right_p_value <- function(vec, obs_stat) {
  mean(vec >= obs_stat)
}

two_sided_p_value <- function(vec, obs_stat) {
  left_pval <- left_p_value(vec, obs_stat)
  right_pval <- right_p_value(vec, obs_stat)
  raw_res <- 2 * min(left_pval, right_pval)

  min(raw_res, 1)
}

check_hypotheses_align <- function(x, obs_stat) {
  if (
    is_hypothesized(x) &&
      is_hypothesized(obs_stat) &&
      any(attr(x, "params") != attr(obs_stat, "params"))
  ) {
    cli_warn(
      "`x` and `obs_stat` were generated using different null hypotheses. \\
        This workflow is untested and results may not mean what you think \\
        they mean."
    )
  }
}

check_x_vs_obs_stat <- function(x, obs_stat, call = caller_env()) {
  # check if x and obs_stat might have been mistakenly supplied
  # in the reverse order
  if (
    is_generated(obs_stat) &&
      !is_generated(x)
  ) {
    cli_abort(
      c(
        "It seems like the `obs_stat` argument has been passed to `get_p_value()` \\
       as the first argument when `get_p_value()` expects `x`, a distribution \\
       of statistics or coefficient estimates, as the first argument. ",
        i = "Have you mistakenly switched the order of `obs_stat` and `x`?"
      ),
      call = call
    )
  }

  invisible(TRUE)
}

# which_distribution <- function(x, theory_type, obs_stat, direction){
#
#   param <- attr(x, "distr_param")
#   if(!is.null(attr(x, "distr_param2")))
#     param2 <- attr(x, "distr_param2")
#
#   if(theory_type == "Two sample t")
#     return(
#       pt(q = obs_stat,
#          df = param,
#          lower.tail = set_lower_tail(direction)
#         )
#     )
# }

#theory_t_pvalue <-

# set_lower_tail <- function(direction){
#   if(direction %in% c("greater", "right"))
#     lower_tail <- FALSE
#   else
#     lower_tail <- TRUE
#
#   lower_tail
# }
