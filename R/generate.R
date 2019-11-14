#' Generate resamples, permutations, or simulations
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("questioning")}
#' 
#' Generation creates a null distribution from [specify()] and (if needed) 
#' [hypothesize()] inputs.
#' 
#' Learn more in `vignette("infer")`.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param reps The number of resamples to generate.
#' @param type Currently either `bootstrap`, `permute`, or `simulate` 
#' (see below).
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
#'   \item `simulate`: A value will be sampled from a theoretical distribution 
#'   with parameters specified in [hypothesize()] for each replicate. (This 
#'   option is currently only applicable for testing point estimates.)
#' }
#'
#' @examples
#' # Generate a null distribution by taking 1000 bootstrap samples
#' gss %>%
#'  specify(response = hours) %>%
#'  hypothesize(null = "point", mu = 40) %>%
#'  generate(reps = 1000, type = "bootstrap")
#' 
#' # Generate a null distribution for the independence of
#' # two variables by permuting their values 1000 times
#' gss %>%
#'  specify(partyid ~ age) %>%
#'  hypothesize(null = "independence") %>%
#'  generate(reps = 1000, type = "permute")
#' 
#' # More in-depth explanation of how to use the infer package
#' vignette("infer")
#'
#' @importFrom dplyr group_by
#' @export
generate <- function(x, reps = 1, type = NULL, ...) {
  type <- sanitize_generation_type(type)
  auto_type <- sanitize_generation_type(attr(x, "type"))
  type <- if(!is.null(type)) { # User specifies type
    compare_type_vs_auto_type(type, auto_type)
  } else { # Use default
    use_auto_type(auto_type)
  }

  attr(x, "generate") <- TRUE

  switch(
    type,
    bootstrap = bootstrap(x, reps, ...),
    permute = {
      check_permutation_attributes(x)
      permute(x, reps, ...)
    },
    simulate = simulate(x, reps, ...)
  )
}


sanitize_generation_type <- function(x) {
  if(is.null(x)) return(x)
  match.arg(x, GENERATION_TYPES)
}

compare_type_vs_auto_type <- function(type, auto_type) {
  if(is.null(auto_type)) {
    # No default; use whatever they specified.
    return(type)
  }
  if (auto_type != type) {
    # User is overriding the default, so warn of potential stupidity.
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
  ## Commented out since no way to currently get to this
  ## All variable types specified have an auto_type set 
  # if(is.null(auto_type)) {
  #   stop_glue(
  #     "There is no default `type`;",
  #     "please set it to one of {toString(shQuote(GENERATION_TYPES))}.",
  #     .sep = " "
  #   )
  # }
  message_glue('Setting `type = "{auto_type}"` in `generate()`.')
  auto_type
}

check_permutation_attributes <- function(x, attr) {
  if (any(is_nuat(x, "response"), is_nuat(x, "explanatory"))) {
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
        col <- as.character(attr(x, "response"))
#      if (attr(x, "theory_type") != "One sample t") {
        x[[col]] <- x[[col]] - mean(x[[col]], na.rm = TRUE) + attr(x, "params")
#      }

      # Standardize after centering above
      #####
      # Determining whether or not to implement this t transformation
      #####
      # else {
      #   std_error <- stats::sd(x[[col]], na.rm = TRUE) /
      #     sqrt(length(x[[col]]))
      #   x[[col]] <- (x[[col]] - mean(x[[col]], na.rm = TRUE)) / std_error
      # }
    }

    # Similarly for median
      else if (identical(attr(attr(x, "params"), "names"), "med")) {
        col <- as.character(attr(x, "response"))
        x[[col]] <- x[[col]] -
          stats::median(x[[col]], na.rm = TRUE) + attr(x, "params")
      }

    # Implement confidence interval for bootstrapped proportions?
    # Implement z transformation?

    # Similarly for sd
    ## Temporarily removed since this implementation does not scale correctly
    # else if (identical(attr(attr(x, "params"), "names"), "sigma")) {
    #   col <- as.character(attr(x, "response"))
    #   x[[col]] <- x[[col]] -
    #     stats::sd(x[[col]], na.rm = TRUE) + attr(x, "params")
    # }
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
    y <- pull(x, !!attr(x, "response"))

    y_prime <- sample(y, size = length(y), replace = FALSE)
    x[as.character(attr(x, "response"))] <- y_prime
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
simulate <- function(x, reps = 1, ...) {
  fct_levels <- as.character(unique(dplyr::pull(x, !!attr(x, "response"))))

  col_simmed <- unlist(replicate(
    reps,
    sample(fct_levels, size = nrow(x), replace = TRUE, prob = format_params(x)),
    simplify = FALSE
  ))

  rep_tbl <- tibble::tibble(
    !!attr(x, "response") := as.factor(col_simmed),
    replicate = as.factor(rep(1:reps, rep(nrow(x), reps)))
  )

  rep_tbl <- copy_attrs(to = rep_tbl, from = x)

  rep_tbl <- dplyr::group_by(rep_tbl, replicate)
  
  append_infer_class(rep_tbl)
}
