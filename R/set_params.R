#' To determine which theoretical distribution to fit (if any)
#' @param x a data frame that can be coerced into a \code{\link[tibble]{tibble}}
set_params <- function(x){

  attr(x, "theory_type") <- NULL
  
  if(!is.null(attr(x, "response"))){
    num_response_levels <- length(levels(x[[as.character(attr(x, "response"))]]))
  }
  
  # One variable
  if (!is.null(attr(x, "response")) && is.null(attr(x, "explanatory")) && 
      !is.null(attr(x, "response_type")) && is.null(attr(x, "explanatory_type"))){
    
    # One mean
    if(attr(x, "response_type") %in% c("integer", "numeric")){
      attr(x, "theory_type") <- "One sample t"
      attr(x, "distr_param") <- x %>% 
        dplyr::summarize(df = stats::t.test(x[[as.character(attr(x, "response"))]])[["parameter"]]) %>% 
        dplyr::pull()
    }
    
    # One prop
    else if(attr(x, "response_type") == "factor" && (num_response_levels == 2)){
      # No parameters since standard normal
      attr(x, "theory_type") <- "One sample prop z"
    } else {
      attr(x, "theory_type") <- "Chi-square Goodness of Fit"
      attr(x, "distr_param") <- num_response_levels - 1
    }
    
  }
  
  # Two variables
  if (!is.null(attr(x, "response")) && !is.null(attr(x, "explanatory")) & 
      !is.null(attr(x, "response_type")) && !is.null(attr(x, "explanatory_type"))){
    # Response is numeric, explanatory is categorical
    if(attr(x, "response_type") %in% c("integer", "numeric") &
       attr(x, "explanatory_type") == "factor"){
      
      # Two sample means (t distribution)
      if(length(levels(x[[as.character(attr(x, "explanatory"))]])) == 2) {
        attr(x, "theory_type") <- "Two sample t"
        # Keep track of Satterthwaite degrees of freedom since lost when in aggregation w/
        # calculate()/generate()
        attr(x, "distr_param") <- x %>% 
          dplyr::summarize(df = stats::t.test(
              !! attr(x, "response") ~ !! attr(x, "explanatory"))[["parameter"]]
            ) %>% 
          dplyr::pull()
      } else {
        # >2 sample means (F distribution)
        attr(x, "theory_type") <- "ANOVA"
        # Get numerator and denominator degrees of freedom
        attr(x, "distr_param") <- x %>% 
          dplyr::summarize(df1 = stats::anova(stats::aov(
              !! attr(x, "response") ~ !! attr(x, "explanatory")))$Df[1]
            ) %>% 
          dplyr::pull()
        attr(x, "distr_param2") <- x %>% 
          dplyr::summarize(df2 = stats::anova(stats::aov(
              !! attr(x, "response") ~ !! attr(x, "explanatory")))$Df[2]
            ) %>% 
          dplyr::pull()
      }
    }
    
    # Response is categorical, explanatory is categorical
    if(attr(x, "response_type") == "factor" &
       attr(x, "explanatory_type") == "factor"){
      
      # Two sample proportions (z distribution) 
      # Parameter(s) not needed since standard normal
      if(length(levels(x[[as.character(attr(x, "response"))]])) == 2 &
         length(levels(x[[as.character(attr(x, "explanatory"))]])) == 2){
        attr(x, "theory_type") <- "Two sample props z"
      }
      # >2 sample proportions (chi-square test of indep)
      else{
        attr(x, "theory_type") <- "Chi-square test of indep"
        attr(x, "distr_param") <- x %>% 
          dplyr::summarize(df = suppressWarnings(stats::chisq.test(
            table(x[[as.character(attr(x, "response"))]],
                  x[[as.character(attr(x, "explanatory"))]]))$parameter)) %>%
          dplyr::pull()
      }
    }
    
    # Response is numeric, explanatory is numeric
    if(attr(x, "response_type") %in% c("integer", "numeric") &
       attr(x, "explanatory_type") %in% c("integer", "numeric")){
      response_string <- as.character(attr(x, "response"))
      explanatory_string <- as.character(attr(x, "explanatory"))
    attr(x, "theory_type") <- "Slope with t"
    attr(x, "distr_param") <- nrow(x) - 2
    }
  }
  
#  if(is.null(attr(x, "theory_type")))
#     warning("Theoretical type not yet implemented")
  
  x
}