#' Generate resamples, permutations, or simulations
#'
#' @description
#' 
#' Generation creates a null distribution from [specify()] and (if needed) 
#' [hypothesize()] inputs.
#' 
#' Learn more in `vignette("infer")`.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param reps The number of resamples to generate.
#' @param type The method used to generate resamples of the observed
#'   data reflecting the null hypothesis. Currently one of 
#'   `"bootstrap"`, `"permute"`, or `"draw"` (see below).
#' @param ... Currently ignored.
#'
#' @return A tibble containing `reps` generated datasets, indicated by the
#'   `replicate` column.
#'   
#' @section Generation Types:
#' 
#' The `type` argument determines the method used to create the null 
#' distribution.
#' 
#' \itemize{
#'   \item `bootstrap`: A bootstrap sample will be drawn for each replicate,
#'   where a sample of size equal to the input sample size is drawn (with 
#'   replacement) from the input sample data.
#'   \item `permute`: For each replicate, each input value will be randomly 
#'   reassigned (without replacement) to a new output value in the sample.
#'   \item `draw`: A value will be sampled from a theoretical distribution 
#'   with parameters specified in [hypothesize()] for each replicate. This 
#'   option is currently only applicable for testing point estimates. This
#'   generation type was previously called `"simulate"`, which has been
#'   superseded.
#' }
#'
#' @examples
#' # Generate a null distribution by taking 200 bootstrap samples
#' gss %>%
#'  specify(response = hours) %>%
#'  hypothesize(null = "point", mu = 40) %>%
#'  generate(reps = 200, type = "bootstrap")
#' 
#' # Generate a null distribution for the independence of
#' # two variables by permuting their values 1000 times
#' gss %>%
#'  specify(partyid ~ age) %>%
#'  hypothesize(null = "independence") %>%
#'  generate(reps = 200, type = "permute")
#' 
#' # More in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @importFrom dplyr group_by
#' @family core functions
#' @export
generate <- function(x, reps = 1, type = NULL, ...) {
  # Check type argument, warning if necessary
  type <- sanitize_generation_type(type)
  auto_type <- sanitize_generation_type(attr(x, "type"))
  type <- if (!is.null(type)) {
    compare_type_vs_auto_type(type, auto_type)
  } else {
    use_auto_type(auto_type)
  }

  attr(x, "generated") <- TRUE

  switch(
    type,
    bootstrap = bootstrap(x, reps, ...),
    permute = {
      check_permutation_attributes(x)
      permute(x, reps, ...)
    },
    draw = draw(x, reps, ...),
    simulate = draw(x, reps, ...)
  )
}

# Check that type argument is an implemented type
sanitize_generation_type <- function(x) {
  if (is.null(x)) return(x)
  
  check_type(x, is.character)

  if (!x %in% c("bootstrap", "permute", "simulate", "draw")) {
    stop_glue(
      'The `type` argument should be one of "bootstrap", "permute", ',
      'or "draw". See `?calculate` for more details.'
    )
  }
  
  if (x == "simulate") {
    message_glue(
      'The "simulate" generation type has been renamed to `type = "draw"`. ',
      'Use `type = "draw"` instead to quiet this message.'
    )
  }
  
  x
}

# Ensure that the supplied type matches what would be assumed from input
compare_type_vs_auto_type <- function(type, auto_type) {
  if(is.null(auto_type)) {
    return(type)
  }
  if (auto_type != type &&
      (any(!c(auto_type, type) %in% c("draw", "simulate")))) {
    warning_glue(
      "You have given `type = \"{type}\"`, but `type` is expected",
      "to be `\"{auto_type}\"`. This workflow is untested and",
      "the results may not mean what you think they mean.",
      .sep = " "
    )
  }
  type
}

use_auto_type <- function(auto_type) {
  message_glue('Setting `type = "{auto_type}"` in `generate()`.')
  auto_type
}

check_permutation_attributes <- function(x, attr) {
  if (any(!has_attr(x, "response"), !has_attr(x, "explanatory"))) {
    stop_glue(
      "Please `specify()` an explanatory and a response variable",
      "when permuting.",
      .sep = " "
    )
  }
}

bootstrap <- function(x, reps = 1, ...) {
  # Check if hypothesis test chosen
  if (is_hypothesized(x)) {
    # If so, shift the variable chosen to have a mean corresponding
    # to that specified in `hypothesize`
    if (!is.null(attr(attr(x, "params"), "names"))){
      if (identical(attr(attr(x, "params"), "names"), "mu")) {
        col <- response_name(x)
        x[[col]] <- x[[col]] - mean(x[[col]], na.rm = TRUE) + attr(x, "params")
    }

    # Similarly for median
      else if (identical(attr(attr(x, "params"), "names"), "med")) {
        col <- response_name(x)
        x[[col]] <- x[[col]] -
          stats::median(x[[col]], na.rm = TRUE) + attr(x, "params")
      }
    }
  }

  # Set variables for use in calculate()
  result <- rep_sample_n(x, size = nrow(x), replace = TRUE, reps = reps)
  result <- copy_attrs(to = result, from = x)

  append_infer_class(result)
}

#' @importFrom dplyr bind_rows group_by
permute <- function(x, reps = 1, ...) {
  df_out <- replicate(reps, permute_once(x), simplify = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(replicate = rep(1:reps, each = nrow(x))) %>%
    dplyr::group_by(replicate)

  df_out <- copy_attrs(to = df_out, from = x)

  append_infer_class(df_out)
}

permute_once <- function(x, ...) {
  dots <- list(...)

  if (is_hypothesized(x) && (attr(x, "null") == "independence")) {
    y <- pull(x, !!response_expr(x))

    y_prime <- sample(y, size = length(y), replace = FALSE)
    x[response_name(x)] <- y_prime
    return(x)
  } else {
    stop_glue(
      "Permuting should be done only when doing independence hypothesis test. ",
      "See `hypothesize()`."
    )
  }
}

#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @importFrom rlang :=
draw <- function(x, reps = 1, ...) {
  fct_levels <- as.character(unique(response_variable(x)))

  col_simmed <- unlist(replicate(
    reps,
    sample(fct_levels, size = nrow(x), replace = TRUE, prob = format_params(x)),
    simplify = FALSE
  ))
  
  x_nrow <- nrow(x)
  rep_tbl <- tibble::tibble(
    !!response_expr(x) := as.factor(col_simmed),
    replicate = as.factor(rep(1:reps, rep(x_nrow, reps)))
  )

  rep_tbl <- copy_attrs(to = rep_tbl, from = x)

  rep_tbl <- dplyr::group_by(rep_tbl, replicate)
  
  append_infer_class(rep_tbl)
}
