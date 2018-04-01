#' Generate resamples, permutations, or simulations based on
#' `specify` and (if needed) `hypothesize` inputs
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param reps the number of resamples to generate
#' @param type currently either \code{bootstrap}, \code{permute}, 
#' or \code{simulate}
#' @param ... currently ignored
#' @return A tibble containing \code{rep} generated datasets, indicated by the
#' \code{replicate} column.
#' @importFrom dplyr group_by
#' @export
#' @examples
#' # Permutation test for two binary variables
#'   mtcars %>%
#'     dplyr::mutate(am = factor(am), vs = factor(vs)) %>%
#'     specify(am ~ vs, success = "1") %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute")

generate <- function(x, reps = 1, type = "bootstrap", ...) {

  if (type == "permute" &&
      any(is.null(attr(x, "response")), is.null(attr(x, "explanatory")))) {
    stop(paste("Please `specify()` an explanatory and a response variable",
         "when permuting."))
  }
  if (type == "simulate" &&
      attr(x, "null") != "point" &&
      !(length(grep("p", names(attr(x, "params")))) >= 1)) {
    stop("Simulation requires a `point` null hypothesis on proportions.")
  }
  if (type == "bootstrap" &&
        !(attr(attr(x, "params"), "names") %in% c("mu", "med", "sigma")) &&
        !is.null(attr(x, "null"))
      ) {
    stop(paste("Bootstrapping is inappropriate in this setting.",
          "Consider using `type = permute` or `type = simulate`."))
  }

  if (type == "bootstrap") {
    return(bootstrap(x, reps, ...))
  }
  if (type == "permute") {
    return(permute(x, reps, ...))
  }
  if (type == "simulate") {
    return(simulate(x, reps, ...))
  }
  if (!(type %in% c("bootstrap", "permute", "simulate")))
    stop(paste("Choose one of the available options for `type`:",
               '`"bootstrap"`, `"permute"`, or `"simulate"`'))
}

bootstrap <- function(x, reps = 1, ...) {
  # Check if hypothesis test chosen
  if(!is.null(attr(x, "null"))){
    # If so, shift the variable chosen to have a mean corresponding
    # to that specified in `hypothesize`
    if(attr(attr(x, "params"), "names") == "mu"){
      
      col <- as.character(attr(x, "response"))
#      if(attr(x, "theory_type") != "One sample t"){
        x[[col]] <- x[[col]] - mean(x[[col]], na.rm = TRUE) + attr(x, "params")
#      }
    
      # Standardize after centering above
      #####
      # Determining whether or not to implement this t transformation
      #####
#      else {
#        std_error <- stats::sd(x[[col]], na.rm = TRUE) / 
#                        sqrt(length(x[[col]]))
#        x[[col]] <- ( x[[col]] - mean(x[[col]], na.rm = TRUE) ) / std_error
#      }
    }

    # Similarly for median
    else if(attr(attr(x, "params"), "names") == "med"){
      col <- as.character(attr(x, "response"))
#      print(median(x[[col]]))
#      print(attr(x, "params"))
      x[[col]] <- x[[col]] -
        stats::median(x[[col]], na.rm = TRUE) + attr(x, "params")
#      print(median(x[[col]]))
    }

    # Implement confidence interval for bootstrapped proportions?
    # Implement z transformation?

    # Similarly for sd
    else if(attr(attr(x, "params"), "names") == "sigma"){
      col <- as.character(attr(x, "response"))
#      print(sd(x[[col]]))
      x[[col]] <- x[[col]] -
        stats::sd(x[[col]], na.rm = TRUE) + attr(x, "params")
#      print(sd(x[[col]]))
    }
  }

  # Set variables for use in calculate()
  result <- rep_sample_n(x, size = nrow(x), replace = TRUE, reps = reps)
  attr(result, "response") <- attr(x, "response")
  attr(result, "success") <- attr(x, "success")
  attr(result, "explanatory") <- attr(x, "explanatory")
  attr(result, "response_type") <- attr(x, "response_type")
  attr(result, "explanatory_type") <- attr(x, "explanatory_type")
  attr(result, "distr_param") <- attr(x, "distr_param")
  attr(result, "distr_param2") <- attr(x, "distr_param2")
  attr(result, "theory_type") <- attr(x, "theory_type")
  

  class(result) <- append("infer", class(result))
  
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
  attr(df_out, "response_type") <- attr(x, "response_type")
  attr(df_out, "explanatory_type") <- attr(x, "explanatory_type")
  attr(df_out, "distr_param") <- attr(x, "distr_param")
  attr(df_out, "distr_param2") <- attr(x, "distr_param2")
  attr(df_out, "theory_type") <- attr(x, "theory_type")
  
  class(df_out) <- append("infer", class(df_out))
  
  # Copy over all attr except for row names
  # attributes(df_out)[which(names(attributes(df_out)) == "row.names")] <-
  #   attributes(x)[which(names(attributes(x)) == "row.names")]
  
  return(df_out)
}

permute_once <- function(x, ...) {
  dots <- list(...)

  if (attr(x, "null") == "independence") {
    y <- pull(x, !! attr(x, "response"))

    y_prime <- sample(y, size = length(y), replace = FALSE)
    x[as.character(attr(x, "response"))] <- y_prime
    return(x)
  }

}

#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @importFrom rlang :=

simulate <- function(x, reps = 1, ...) {
  fct_levels <- as.character(unique(dplyr::pull(x, !! attr(x, "response"))))

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
  attr(rep_tbl, "response_type") <- attr(x, "response_type")
  attr(rep_tbl, "explanatory_type") <- attr(x, "explanatory_type")
  attr(rep_tbl, "distr_param") <- attr(x, "distr_param")
  attr(rep_tbl, "distr_param2") <- attr(x, "distr_param2")
  attr(rep_tbl, "theory_type") <- attr(x, "theory_type")
  
  class(rep_tbl) <- append("infer", class(rep_tbl))
  
  # Copy over all attr except for row names
  #attributes(rep_tbl)[which(names(attributes(rep_tbl)) == "row.names")] <-
  #  attributes(x)[which(names(attributes(x)) == "row.names")]
  
  
  # TODO: we may want to clean up this object before sending it out - do we
  # really need all of the attributes() that it spits out?
  
  ## From Chester: Upon further inspection, I think we'll need a bunch of 
  ## these to appropriately determine the theoretical distributions
  ## when they exist

  return(dplyr::group_by(rep_tbl, replicate))
}

#' @importFrom dplyr pull

format_params <- function(x) {
  par_levels <- get_par_levels(x)
  fct_levels <- as.character(unique(dplyr::pull(x, !! attr(x, "response"))))
  return(attr(x, "params")[match(fct_levels, par_levels)])
}

get_par_levels <- function(x) {
  par_names <- names(attr(x, "params"))
  return(gsub("^.\\.", "", par_names))
}
