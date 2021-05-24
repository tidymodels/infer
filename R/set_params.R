#' To determine which theoretical distribution to fit (if any)
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#'
#' @noRd
set_params <- function(x) {
  attr(x, "theory_type") <- NULL
  
  if (has_response(x)) {
    num_response_levels <- length(unique(response_variable(x)))
    
    check_factor_levels(
      response_variable(x), 
      "response", 
      response_name(x)
    )
  }
  
  if (is_mlr(x)) {
    return(x)
  }
  
  if (has_explanatory(x)) {
    num_explanatory_levels <- length(unique(explanatory_variable(x)))
    
    check_factor_levels(
      explanatory_variable(x), 
      "explanatory", 
      explanatory_name(x)
    )
  }
  
  # One variable
  if (
    has_response(x) && !has_explanatory(x) &&
    has_attr(x, "response_type") && !has_attr(x, "explanatory_type")
  ) {
    
    # One mean
    if (attr(x, "response_type") %in% c("integer", "numeric")) {
      attr(x, "theory_type") <- "One sample t"
      attr(x, "distr_param") <- stats::t.test(
        response_variable(x)
      )[["parameter"]]
      attr(x, "type") <- "bootstrap"
    } else if (
      
      # One prop
      (attr(x, "response_type") == "factor") && (num_response_levels == 2)
    ) {
      # No parameters since standard normal
      attr(x, "theory_type") <- "One sample prop z"
      # Changed to `"simulate"` when `p` provided in `hypothesize()`
      attr(x, "type") <- "bootstrap"
    } else {
      attr(x, "theory_type") <- "Chi-square Goodness of Fit"
      attr(x, "distr_param") <- num_response_levels - 1
      attr(x, "type") <- "simulate"
    }
  }
  
  # Two variables
  if (
    has_response(x) && has_explanatory(x) &
    has_attr(x, "response_type") && has_attr(x, "explanatory_type")
  ) {
    attr(x, "type") <- "bootstrap"
    
    # Response is numeric, explanatory is categorical
    if (
      (attr(x, "response_type") %in% c("integer", "numeric")) &
      (attr(x, "explanatory_type") == "factor")
    ) {
      
      # Two sample means (t distribution)
      if (num_explanatory_levels == 2) {
        attr(x, "theory_type") <- "Two sample t"
        # Keep track of Satterthwaite degrees of freedom since lost when
        # in aggregation w/ calculate()/generate()
        attr(x, "distr_param") <- stats::t.test(
          response_variable(x) ~ explanatory_variable(x)
        )[["parameter"]]
      } else {
        
        # >2 sample means (F distribution)
        attr(x, "theory_type") <- "ANOVA"
        # Get numerator and denominator degrees of freedom
        degrees <- stats::anova(stats::aov(
          response_variable(x) ~ explanatory_variable(x)
        ))$Df
        attr(x, "distr_param") <- degrees[1]
        attr(x, "distr_param2") <- degrees[2]
      }
    }
    
    # Response is categorical, explanatory is categorical
    if (
      (attr(x, "response_type") == "factor") &
      (attr(x, "explanatory_type") == "factor")
    ) {
      attr(x, "type") <- "bootstrap"
      
      # Two sample proportions (z distribution)
      # Parameter(s) not needed since standard normal
      if (
        (num_response_levels == 2) &
        (num_explanatory_levels == 2)
      ) {
        attr(x, "theory_type") <- "Two sample props z"
      } else {
        
        # >2 sample proportions (chi-square test of indep)
        attr(x, "theory_type") <- "Chi-square test of indep"
        attr(x, "distr_param") <- suppressWarnings(
          stats::chisq.test(
            table(response_variable(x), explanatory_variable(x))
          )$parameter
        )
      }
    }
    
    # Response is numeric, explanatory is numeric
    if (
      (attr(x, "response_type") %in% c("integer", "numeric")) &
      (attr(x, "explanatory_type") %in% c("integer", "numeric"))
    ) {
      response_string <- response_name(x)
      explanatory_string <- explanatory_name(x)
      attr(x, "theory_type") <- "Slope/correlation with t"
      attr(x, "distr_param") <- nrow(x) - 2
    }
  }

  x
}

check_factor_levels <- function(x, type, name) {
  if (is.factor(x)) {
    unused <- setdiff(levels(x), unique(x))
    
    if (length(unused) > 0) {
      message_glue(
        "Dropping unused factor levels {list(unused)} from the ",
        "supplied {type} variable '{name}'."
      )
    }
  }
}
