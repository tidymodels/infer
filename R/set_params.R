#' To determine which theoretical distribution to fit (if any)
#' @param x a data frame that can be coerced into a \code{\link[tibble]{tibble}}
set_params <- function(x){
  # Two sample means (t distribution)
  # Keep track of Satterthwaite degrees of freedom since lost when in aggregation w/
  # calculate()/generate()
  if (!is.null(attr(x, "response")) & !is.null(attr(x, "explanatory")) & 
      !is.null(attr(x, "response_type")) & !is.null(attr(x, "explanatory_type"))){
    if(attr(x, "response_type") %in% c("integer", "numeric") &
       attr(x, "explanatory_type") == "factor"){
      if(length(levels(x[[as.character(attr(x, "explanatory"))]])) == 2) {
        attr(x, "distr_param") <- x %>% 
          dplyr::summarize(stats::t.test(!! attr(x, "response") ~ !! attr(x, "explanatory"))[["parameter"]]) %>% 
          dplyr::pull()
      }
    }
  }
  x
}