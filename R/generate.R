#' Generate resamples
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param reps the number of resamples to generate
#' @param type currently either \code{bootstrap}, \code{permute}, or \code{simulate}
#' @param ... currently ignored
#' @importFrom dplyr group_by
#' @export
#' @examples
#'
#' #' # Permutation test for two binary variables
#' if (require(dplyr)) {
#'   mtcars %>%
#'     mutate(am = factor(am), vs = factor(vs)) %>%
#'     specify(am ~ vs, success = "1") %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute")
#' }

generate <- function(x, reps = 1, type = "bootstrap", ...) {
  
  ## Need assertion here that `type = "permute"` requires 
  ## that both the `explanatory` and `response` attributes to be set
  
  if (type == "bootstrap") {
    return(bootstrap(x, reps, ...))
  }
  if (type == "permute") {
    return(permute(x, reps, ...))
  }
  if (type == "simulate") {
    return(simulate(x, reps, ...))
  }
  x
}

bootstrap <- function(x, reps = 1, ...) {
  # Check if hypothesis test chosen
  if(!is.null(attr(x, "null"))){
    # If so, shift the variable chosen to have a mean corresponding
    # to that specified in `hypothesize`
    if(attr(attr(x, "params"), "names") == "mu"){
      col <- as.character(attr(x, "response"))
      x[[col]] <- x[[col]] - mean(x[[col]], na.rm = TRUE) + attr(x, "params")
    }

    # Similarly for median
    if(attr(attr(x, "params"), "names") == "Med"){
      col <- as.character(attr(x, "response"))
      x[[col]] <- x[[col]] - stats::median(x[[col]], na.rm = TRUE) + attr(x, "params")
    }

    # Similarly for sd
    if(attr(attr(x, "params"), "names") == "sd"){
      col <- as.character(attr(x, "response"))
      x[[col]] <- x[[col]] - stats::sd(x[[col]], na.rm = TRUE) + attr(x, "params")
    }

    # Similarly for sd
    if(attr(attr(x, "params"), "names") == "sd"){
      col <- as.character(attr(x, "response"))
      x[[col]] <- x[[col]] - stats::sd(x[[col]]) + attr(x, "params")
    }
  }

  # Set variables for use in calculate()
  result <- rep_sample_n(x, size = nrow(x), replace = TRUE, reps = reps)
  attr(result, "response") <- attr(x, "response")
  attr(result, "success") <- attr(x, "success")
  attr(result, "explanatory") <- attr(x, "explanatory")

  return(result)
}

#' @importFrom dplyr bind_rows group_by

permute <- function(x, reps = 1, ...) {
  df_out <- replicate(reps, permute_once(x), simplify = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(replicate = rep(1:reps, each = nrow(x))) %>%
    dplyr::group_by(replicate)
  attr(df_out, "null") <- attr(x, "null")
  attr(df_out, "response") <- attr(x, "response")
  attr(df_out, "success") <- attr(x, "success")
  attr(df_out, "explanatory") <- attr(x, "explanatory")
  return(df_out)
}

permute_once <- function(x, ...) {
  dots <- list(...)

  if (attr(x, "null") == "independence") {
    y <- pull(x, !! attr(x, "response"))

    y_prime <- sample(y, size = length(y), replace = TRUE)
    x[as.character(attr(x, "response"))] <- y_prime
    return(x)
  }

}

#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @importFrom rlang :=

simulate <- function(x, reps = 1, ...) {
  fct_levels <- levels(dplyr::pull(x, !! attr(x, "response")))

  col_simmed <- unlist(replicate(reps, sample(fct_levels,
                                              size = nrow(x),
                                              replace = TRUE,
                                              prob = format_params(x)),
                                 simplify = FALSE))

  rep_tbl <- tibble(!! attr(x, "response") := as.factor(col_simmed),
                   replicate = as.factor(rep(1:reps, rep(nrow(x), reps))))

  attr(rep_tbl, "null") <- attr(x, "null")
  attr(rep_tbl, "params") <- attr(x, "params")
  attr(rep_tbl, "response") <- attr(x, "response")
  attr(rep_tbl, "success") <- attr(x, "success")
  attr(rep_tbl, "explanatory") <- attr(x, "explanatory")
  #  attr(rep_tbl, "ci") <- attr(tbl, "ci")
  # TODO: we may want to clean up this object before sending it out - do we
  # really need all of the attributes() that it spits out?
  return(dplyr::group_by(rep_tbl, replicate))
}

#' @importFrom dplyr pull

format_params <- function(x) {
  par_levels <- get_par_levels(x)
  fct_levels <- levels(dplyr::pull(x, !! attr(x, "response")))
  return(attr(x, "params")[match(fct_levels, par_levels)])
}

get_par_levels <- function(x) {
  par_names <- names(attr(x, "params"))
  return(gsub("^.\\.", "", par_names))
}

#' @importFrom dplyr as_tibble pull data_frame inner_join

# Modified oilabs::rep_sample_n() with attr added
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1, prob = NULL) {
  n <- nrow(tbl)

  # assign non-uniform probabilities
  # there should be a better way!!
  # prob needs to be nrow(tbl) -- not just number of factor levels
  if (!is.null(prob)) {
    if (length(prob) != n) stop("The argument prob must have length nrow(tbl).")
    df_lkup <- dplyr::data_frame(vals = levels(dplyr::pull(tbl, 1)))
    names(df_lkup) <- names(tbl)
    df_lkup$probs = prob
    tbl_wgt <- dplyr::inner_join(tbl, df_lkup)
    prob <- tbl_wgt$probs
  }

  i <- unlist(replicate(reps, sample.int(n, size, replace = replace, prob = prob),
                        simplify = FALSE))
  rep_tbl <- cbind(replicate = rep(1:reps, rep(size, reps)),
                   tbl[i, ])
  rep_tbl <- dplyr::as_tibble(rep_tbl)
  names(rep_tbl)[-1] <- names(tbl)
  #  attr(rep_tbl, "ci") <- attr(tbl, "ci")
  dplyr::group_by(rep_tbl, replicate)
}
