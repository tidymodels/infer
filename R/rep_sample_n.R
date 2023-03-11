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
#' The `size` argument to `rep_sample_n()` is required, while in
#' `rep_slice_sample()` sample size defaults to 1 if not specified. `prop`, an
#' argument to `rep_slice_sample()`, refers to the proportion of rows to sample
#' in each sample, and is rounded down in the case that `prop * nrow(.data)` is
#' not an integer. When using `rep_slice_sample()`, please only supply one of
#' `n` or `prop`.
#' @param replace Should samples be taken with replacement?
#' @param reps Number of samples to take.
#' @param prob,weight_by A vector of sampling weights for each of the rows in
#' `.data`—must have length equal to `nrow(.data)`. For `weight_by`, this
#' may also be an unquoted column name in `.data`.
#'
#' @details
#'
#' `rep_sample_n()` and `rep_slice_sample()` are designed to behave similar to
#' their dplyr counterparts. As such, they have at least the following
#' differences:
#' - In case `replace = FALSE` having `size` bigger than number of data rows in
#' `rep_sample_n()` will give an error. In `rep_slice_sample()` having such `n`
#' or `prop > 1` will give warning and output sample size will be set to number
#' of rows in data.
#'
#' Note that the [dplyr::sample_n()] function  has been superseded by
#' [dplyr::slice_sample()].
#'
#' @return A tibble of size `reps * n` rows corresponding to `reps`
#'   samples of size `n` from `.data`, grouped by `replicate`.
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
#'
#' # alternatively, pass an unquoted column name in `.data` as `weight_by`
#' df <- df %>% mutate(wts = c(.5, .4, .3, .2, .1))
#'
#' rep_slice_sample(df, n = 2, reps = 5, weight_by = wts)
#' @export
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1, prob = NULL) {
  check_type(tbl, is.data.frame)
  check_type(size, is_single_number, "single non-negative number", min_val = 0)
  check_type(replace, is_truefalse, "TRUE or FALSE")
  check_type(
    reps,
    is_single_number,
    "single number not less than 1",
    min_val = 1
  )
  check_type(
    prob,
    ~ is.numeric(.) && (length(.) == nrow(tbl)),
    glue::glue("numeric vector with length `nrow(tbl)` = {nrow(tbl)}"),
    allow_null = TRUE
  )

  # In `dplyr::sample_n()` `size` can't be more than number of rows in data
  notify_extra_size(size, tbl, replace, notify_type = "sample_n")

  make_replicate_tbl(
    tbl = tbl,
    size = size,
    replace = replace,
    prob = prob,
    reps = reps
  )
}

#' @rdname rep_sample_n
#' @export
rep_slice_sample <- function(.data, n = NULL, prop = NULL, replace = FALSE,
                             weight_by = NULL, reps = 1) {
  check_type(.data, is.data.frame)
  check_type(
    n,
    is_single_number,
    "single non-negative number",
    allow_null = TRUE,
    min_val = 0
  )
  check_type(
    prop,
    is_single_number,
    "single non-negative number",
    allow_null = TRUE,
    min_val = 0
  )
  check_type(replace, is_truefalse, "TRUE or FALSE")
  eval_weight_by <- try(rlang::eval_tidy(weight_by), silent = TRUE)
  if (inherits(eval_weight_by, "try-error")) {
     weight_by <- rlang::enquo(weight_by)
     check_cols(.data, weight_by, "permute", FALSE, "weight_by")
     weight_by <- .data[[rlang::as_name(weight_by)]]
  }
  check_type(
     weight_by,
     ~ is.numeric(.) && (length(.) == nrow(.data)),
     glue::glue("a numeric vector with length `nrow(.data)` = {nrow(.data)} \\
                 or an unquoted column name"),
     allow_null = TRUE
  )
  check_type(
    reps,
    is_single_number,
    "single number not less than 1",
    min_val = 1
  )

  # Compute sample size based on `n` and `prop`
  size <- make_slice_size(n = n, prop = prop, n_total = nrow(.data))

  # In `dplyr::slice_sample()` asked sample size is allowed to be bigger than
  # number of rows in data. In that case (at least currently) sample size is
  # silently replaced to be number of rows. Here we give a warning.
  notify_extra_size(size, .data, replace, notify_type = "slice_sample")

  make_replicate_tbl(
    tbl = .data,
    size = size,
    replace = replace,
    prob = weight_by,
    reps = reps
  )
}

make_replicate_tbl <- function(tbl, size, replace, prob, reps) {
  # NOTE: this implementation is a way faster alternative to using
  # `purrr::map_dfr()` + `dplyr::slice_sample()` (or other sampling function)

  # Generate row indexes for every future replicate (this way it respects
  # possibility of `replace = FALSE`)
  n <- nrow(tbl)
  idx_list <- replicate(
    reps,
    sample_int(n, size, replace = replace, prob = prob),
    simplify = FALSE
  )
  # Get actual sample size which can differ from `size` (currently if it is
  # bigger than number of rows in `tbl` inside `rep_slice_sample()`)
  sample_size <- length(idx_list[[1]])
  i <- unlist(idx_list)

  tbl %>%
    dplyr::slice(i) %>%
    dplyr::mutate(replicate = rep(seq_len(reps), each = sample_size)) %>%
    dplyr::select(replicate, dplyr::everything()) %>%
    tibble::as_tibble() %>%
    dplyr::group_by(replicate)
}

notify_extra_size <- function(size, tbl, replace, notify_type) {
  if (!replace && (size > nrow(tbl))) {
    msg <- glue::glue(
      "Asked sample size ({size}) is bigger than ",
      "number of rows in data ({nrow(tbl)}) while `replace` is FALSE"
    )
    switch(
      notify_type,
      sample_n = stop_glue("{msg}. Use `replace = TRUE`."),
      slice_sample = warning_glue("{msg}. Using number of rows as sample size.")
    )
  }

  TRUE
}

# Modified code from https://github.com/tidyverse/dplyr/blob/master/R/slice.R
# (at commit 0f29aa4)
sample_int <- function(n, size, replace = FALSE, prob = NULL) {
  if (!replace) {
    # If `replace` is `FALSE`, allow `size` to be bigger than `n` by silently
    # replacing it with `n`
    size <- min(size, n)
  }

  if (size == 0L) {
    integer(0)
  } else {
    sample.int(n, size, prob = prob, replace = replace)
  }
}

make_slice_size <- function(n, prop, n_total) {
  if (is.null(n)) {
    if (is.null(prop)) {
      # By default return size 1
      1L
    } else {
      as.integer(n_total * prop)
    }
  } else {
    if (is.null(prop)) {
      n
    } else {
      stop_glue("Please supply exactly one of the `n` or `prop` arguments.")
    }
  }
}
