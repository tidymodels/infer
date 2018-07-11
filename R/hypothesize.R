#' Declare a null hypothesis
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param null the null hypothesis. Options include "independence" and "point"
#' @param ... arguments passed to downstream functions
#' @return A tibble containing the response (and explanatory, if specified) 
#' variable data with parameter information stored as well
#' @importFrom dplyr as.tbl
#' @return a data frame with attributes set
#' @export
#' @examples
#' # Permutation test similar to ANOVA
#'   mtcars %>%
#'     dplyr::mutate(cyl = factor(cyl)) %>%
#'     specify(mpg ~ cyl) %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "F")

hypothesize <- function(x, null, ...) {
  
  hypothesize_checks(x, null)
  
  attr(x, "null") <- null
  
  dots <- list(...)
  
  if( (null == "point") && (length(dots) == 0) ){
    stop(paste("Provide a parameter and a value to check such as `mu = 30`",
               "for the point hypothesis."))
  }
  
  if((null == "independence") && (length(dots) > 0)) {
    warning(paste("Parameter values are not specified when testing that two",
                  "variables are independent."))
  }
  
  if((length(dots) > 0) && (null == "point")) {
    params <- parse_params(dots, x)
    attr(x, "params") <- params
    
    if(any(grepl("p.", attr(attr(x, "params"), "names")))){
     # simulate instead of bootstrap based on the value of `p` provided
      attr(x, "type") <- "simulate"
    } else {
      attr(x, "type") <- "bootstrap"
    }
    
  }
  
  if(!is.null(null) && null == "independence")
    attr(x, "type") <- "permute"
  
  # Check one proportion test set up correctly
  if(null == "point"){
    if(is.factor(response_variable(x))){
      if(!any(grepl("p", attr(attr(x, "params"), "names"))))
        stop(paste('Testing one categorical variable requires `p`',
                   'to be used as a parameter.'))
    }
  }
  
  # Check one numeric test set up correctly
  ## Not currently able to reach in testing as other checks
  ## already produce errors
  # if(null == "point"){
  #   if(!is.factor(response_variable(x))
  #      & !any(grepl("mu|med|sigma", attr(attr(x, "params"), "names"))))
  #     stop(paste('Testing one numerical variable requires one of',
  #                '`mu`, `med`, or `sd` to be used as a parameter.'))
  # }
  
  return(as.tbl(x))
}

