#' Generate resamples
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param reps the number of resamples to generate
#' @param type currently either \code{bootstrap} or \code{permute}
#' @param ... currently ignored
#' @importFrom dplyr group_by
#' @export

generate <- function(x, reps = 1, type = "bootstrap", ...) {
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
  rep_sample_n(x, size = nrow(x), replace = TRUE, reps = reps)
}

#' @importFrom dplyr bind_rows mutate_ group_by

permute <- function(x, reps = 1, ...) {
  df_out <- replicate(reps, permute_once(x), simplify = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate_(replicate = ~rep(1:reps, each = nrow(x))) %>%
    dplyr::group_by(replicate)
  attr(df_out, "null") <- attr(x, "null")
  return(df_out)
}

permute_once <- function(x, ...) {
  dots <- list(...)

  if(attr(x, "null") == "equal means"){
    ## need to look for name of variable to permute...ugh
    ## by default, use the first column
    # y <- x[, 1]
    ## Hopefully this fixes that
    num_cols <- sapply(x, is.numeric)
    num_name <- names(num_cols[num_cols == TRUE])
    y <- x[[num_name]]

    y_prime <- y[ sample.int(length(y)) ]
    x[[num_name]] <- y_prime
    return(x)
  }

  if(attr(x, "null") == "independence"){
    ## by default, permute the first column of the two selected
    # Since dealing with tibble potentially, we need to force a
    # vector here
    y <- x[[1]]

    y_prime <- y[ sample.int(length(y)) ]
    x[[1]] <- y_prime
    return(x)
  }

}

simulate <- function(x, reps = 1, ...) {
  # error
  if (nrow(x) > 1 | class(x[1]) != "factor") {
    stop("Simulation can only be performed for a single categorical variable.")
  }

  sample(levels(x[1]), length(x[1]), prob = unlist(attr(x, params)))
  # INCOMPLETE
}

#' @importFrom dplyr as_tibble

# Modified oilabs::rep_sample_n() with attr added
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1) {
  # attr(tbl, "ci") <- TRUE
  n <- nrow(tbl)
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace),
                        simplify = FALSE))
  rep_tbl <- cbind(replicate = rep(1:reps, rep(size, reps)),
                   tbl[i, ])
  rep_tbl <- dplyr::as_tibble(rep_tbl)
  names(rep_tbl)[-1] <- names(tbl)
  #  attr(rep_tbl, "ci") <- attr(tbl, "ci")
  dplyr::group_by(rep_tbl, replicate)
}
