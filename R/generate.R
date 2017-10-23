#' Generate resamples of a type provided via \code{type} parameter
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param reps the number of resamples to generate
#' @param type currently either \code{bootstrap}, \code{permute}, or \code{simulate}
#' @param ... currently ignored
#' @importFrom dplyr group_by
#' @export
#' @examples
#' # Generate 100 bootstrap means
#' # resulting in 100 * 32 = 3200 rows
#' mtcars %>%
#'    specify(response = mpg) %>%
#'    generate(reps = 100, type = "bootstrap")
#'    
#' # Generate 200 permutations of two proportions example
#' # resulting in 200 * 32 = 6400 rows
#' if(require(dplyr)) {
#'     mtcars %>%
#'         mutate(am = factor(am), vs = factor(vs)) %>%
#'         specify(am ~ vs) %>% # alt: response = am, explanatory = vs
#'         hypothesize(null = "independence") %>%
#'         generate(reps = 200, type = "permute")
#'  }
#'  
#' # Generate 150 simulations assuming true probability
#' # of success (a "1") is 0.25,
#' # resulting in 150 * 32 = 4800 rows
#' if(require(dplyr)) {
#'     mtcars %>%
#'         mutate(am = factor(am)) %>%
#'         specify(response = am) %>% # alt: am ~ NULL (or am ~ 1)
#'         hypothesize(null = "point", p = c("1" = .25)) %>% 
#'         generate(reps = 150, type = "simulate") 
#' }

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

    # TODO: Similarly for t

    # TODO: Similarly for z  
  }
  
  # Set variables for use in calculate()
  result <- rep_sample_n(x, size = nrow(x), replace = TRUE, reps = reps)
  attr(result, "response") <- attr(x, "response")
  attr(result, "explanatory") <- attr(x, "explanatory")
  attr(result, "response_type") <- attr(x, "response_type")
  attr(result, "explanatory_type") <- attr(x, "explanatory_type")
  attr(result, "distr_param") <- attr(x, "distr_param")
  attr(result, "distr_param2") <- attr(x, "distr_param2")
  attr(result, "theory_type") <- attr(x, "theory_type")
  
  # Copy over all attr except for row names
  # attributes(result)[which(names(attributes(result)) == "row.names")] <-
  #   attributes(x)[which(names(attributes(x)) == "row.names")]
  
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
  attr(df_out, "explanatory") <- attr(x, "explanatory")
  attr(df_out, "response_type") <- attr(x, "response_type")
  attr(df_out, "explanatory_type") <- attr(x, "explanatory_type")
  attr(df_out, "distr_param") <- attr(x, "distr_param")
  attr(df_out, "distr_param2") <- attr(x, "distr_param2")
  attr(df_out, "theory_type") <- attr(x, "theory_type")
  
  # Copy over all attr except for row names
  # attributes(df_out)[which(names(attributes(df_out)) == "row.names")] <-
  #   attributes(x)[which(names(attributes(x)) == "row.names")]
  
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
  attr(rep_tbl, "explanatory") <- attr(x, "explanatory")
  attr(rep_tbl, "response_type") <- attr(x, "response_type")
  attr(rep_tbl, "explanatory_type") <- attr(x, "explanatory_type")
  attr(rep_tbl, "distr_param") <- attr(x, "distr_param")
  attr(rep_tbl, "distr_param2") <- attr(x, "distr_param2")
  attr(rep_tbl, "theory_type") <- attr(x, "theory_type")
  
  # Copy over all attr except for row names
  #attributes(rep_tbl)[which(names(attributes(rep_tbl)) == "row.names")] <-
  #  attributes(x)[which(names(attributes(x)) == "row.names")]
  
  
  # TODO: we may want to clean up this object before sending it out - do we
  # really need all of the attributes() that it spits out?
  
  ## From Chester: Upon further inspection, I think we'll need a bunch of these to 
  ## appropriately determine the theoretical distributions when they exist
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
