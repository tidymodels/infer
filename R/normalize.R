#' Normalize numerical data to zero mean and unitary variance
#'
#'
#' @param .data A tibble or dataframe object
#' @param .integers logical. Should integer vectors be normalized too? Defaults to `FALSE`.
#' 
#' @import rlang
#' @import purrr
#' @import dplyr
#'
#' @return A tibble with normalized numerical data
#'
#' @export
#'
#' @examples 
#' data <- tibble(A = rnorm(n = 1000), B = rt(n = 1000, df = 2)) %>% normalize()
#' kmeans(data)
#' @author Pedro Cavalcante \email{pedro.cavalcante@@mutual.club}

normalize <- function(.data, .integers = FALSE) {
  
  require(rlang, quietly = TRUE)
  require(purrr, quietly = TRUE)
  
  classes = tibble(variables = names(.data),
                   keep = FALSE)
  
  for(i in 1:nrow(classes)) {
    
    var <- sym(classes$variables[i])
    
    if(.integers == TRUE) {
   
      valid <- (.data %>% 
        pull({{var}}) %>%
        is_double() | 
          
        .data %>% 
          pull({{var}}) %>% 
          is_integer() )
      
    } else {
      
      valid <- .data %>% 
        pull({{var}}) %>% 
        is_double()
      
    }
    
    classes$keep[i] <- valid
  
    rm(valid)
  
  }
  
 variables <- classes %>% 
   filter(keep == TRUE) %>%
   pull(variables) 
 
 for(i in 1:length(variables)) {
  
   var <- sym(variables[i])
   
   v <- .data %>% pull({{var}}) 
   
   .data <- .data %>% 
     mutate({{var}} := ((v - mean(v, na.rm = TRUE) )  / sd(v, na.rm = TRUE)) )  
   
  }
   
  
 return(.data)
 
}
