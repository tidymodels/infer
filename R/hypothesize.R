#' Declare a null hypothesis
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param null the null hypothesis. Options include "independence" and "point"
#' @param ... arguments passed to downstream functions
#' @return A tibble containing the response (and explanatory, if specified) variable data with
#' parameter information stored as well
#' @importFrom dplyr as.tbl
#' @export
#' @examples
#' # One binary variable
#'   mtcars %>%
#'     dplyr::mutate(am = factor(am)) %>%
#'     specify(response = am, success = "1") %>%
#'     hypothesize(null = "point", p = 0.75) %>%
#'     generate(reps = 100, type = "simulate") %>%
#'     calculate(stat = "prop")
#'
#' # Permutation test
#'   mtcars %>%
#'     dplyr::mutate(cyl = factor(cyl)) %>%
#'     specify(mpg ~ cyl) %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "F")

hypothesize <- function(x, null, ...) {
  
  # error: x is not a dataframe
  if (!sum(class(x) %in% c("data.frame", "tbl", "tbl_df", "grouped_df"))) {
    stop("x must be a data.frame or tibble")
  }
  
  # error: null not found
  if (!(null %in% c("independence", "point"))) {
    stop("Choice of null is not supported. Check `?hypothesize` for options.")
  }
  
  #  if (length(null) != 1) {
  #    stop(paste0('Choose between either `"independence"` or `"point"`',
  #                 'for `null` argument.')
  #  }
  
  if(is.null(attr(x, "response"))){
    stop(paste("The response variable is not set.",
               "Make sure to `specify()` it first."))
  }
  
  if(null == "independence" & is.null(attr(x, "explanatory"))){
    stop(paste0('Please `specify()` an explanatory and a response variable',
                'when testing \n',
                'a null hypothesis of `"independence"`.'))
  }
  
  attr(x, "null") <- null
  
  dots <- list(...)
  
  if( (null == "point") & (length(dots) == 0) ){
    stop(paste("Provide a parameter and a value to check such as `mu = 30`",
               "for the point hypothesis."))
  }
  
  if((null == "independence") & (length(dots) > 0)) {
    warning(paste("Parameter values are not specified when testing that two",
                  "variables are independent."))
  }
  
  if ((length(dots) > 0) && (null == "point")) {
    params <- parse_params(dots, x)
    attr(x, "params") <- params
  }
  
  # Check one proportion test set up correctly
  if(null == "point"){
    if(is.factor(x[[as.character(attr(x, "response"))]])){
      if(!any(grepl("p", attr(attr(x, "params"), "names"))))
        stop(paste('Testing one categorical variable requires `p`",
                   "to be used as a parameter.'))
    }
  }
  
  # Check one numeric test set up correctly
  if(null == "point"){
    if(!is.factor(x[[as.character(attr(x, "response"))]])
       & !any(grepl("mu|med|sigma", attr(attr(x, "params"), "names"))))
      stop(paste('Testing one numerical variable requires one of',
                 '`mu`, `med`, or `sd` to be used as a parameter.'))
  }
  
  return(as.tbl(x))
}

parse_params <- function(dots, x) {
  p_ind <- grep("p", names(dots))
  mu_ind <- grep("mu", names(dots))
  med_ind <- grep("med", names(dots))
  sig_ind <- grep("sigma", names(dots))
  
  # error: cannot specify more than one of props, means, medians, or sds
  if ( length(p_ind) + length(mu_ind) + length(med_ind) 
       + length(sig_ind) != 1 ){
    stop('Parameter values can be only one of `p`, `mu`, `med`, or `sigma`.')
  }
  
  # add in 1 - p if it's missing
  # Outside if() is needed to ensure an error does not occur in referencing the
  # 0 index of dots
  if (length(p_ind)) {
    if (length(dots[[p_ind]]) == 1) {
      if (attr(x, "null") == "point" && is.null(attr(x, "success"))) {
        stop(paste("A point null regarding a proportion requires",
                   "that `success` be indicated in `specify()`."))
      }
      if(dots$p < 0 | dots$p > 1)
        stop("The value suggested for `p` is not between 0 and 1, inclusive.")
      missing_lev <- setdiff(unique(pull(x, !!attr(x, "response"))), 
                             attr(x, "success"))
      dots$p <- append(dots$p, 1 - dots$p)
      names(dots$p) <- c(attr(x, "success"), missing_lev)
    } else {
      if(sum(dots$p) != 1){
        stop(paste("Make sure the hypothesized values for the `p` parameters",
                   "sum to 1. Please try again."))
      }
    }
  }
  
  # if (sum(dots[[p_ind]]) != 1){
  #   dots[[p_ind]] <- dots[[p_ind]]/sum(dots[[p_ind]])
  #   warning("Proportions do not sum to 1, normalizing automatically.")
  # }
  
  return(unlist(dots))
}
