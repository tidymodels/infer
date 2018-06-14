#' Compute the p-value for (currently only) simulation-based methods
#' \code{get_pvalue()} is an alias of \code{p_value}
#' @param x data frame of calculated statistics or containing attributes
#' of theoretical distribution values
#' @param obs_stat a numeric value or a 1x1 data frame (as extreme or more extreme than this)
#' @param direction a character string. Options are "less", "greater", or "two_sided".
#' Can also specify "left", "right", or "both".
#'
#' @return a 1x1 data frame with value between 0 and 1
#' @export
#' @rdname get_pvalue
#' @examples
#' mtcars_df <- mtcars %>%
#'     dplyr::mutate(am = factor(am))
#' d_hat <- mtcars_df %>%
#'     specify(mpg ~ am) %>%
#'     calculate(stat = "diff in means", order = c("1", "0"))
#' null_distn <- mtcars_df %>%
#'     specify(mpg ~ am) %>% 
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100) %>%
#'     calculate(stat = "diff in means", order = c("1", "0"))
#' null_distn %>% 
#'     p_value(obs_stat = d_hat, direction = "right")

p_value <- function(x, obs_stat, direction){
  
  assertive::assert_is_data.frame(x)
  obs_stat <- check_obs_stat(obs_stat)
  check_direction(direction)
  
  is_simulation_based <- !is.null(attr(x, "generate")) &&
    attr(x, "generate")

  if(is_simulation_based)
    pvalue <- simulation_based_p_value(x = x, obs_stat = obs_stat, 
                                        direction = direction)
  
  ## Theoretical-based p-value
  # Could be more specific
  # else if(is.null(attr(x, "theory_type")) || is.null(attr(x, "distr_param")))
  #   stop(paste("Attributes have not been set appropriately.",
  #              "Check your {infer} pipeline again."
  #   ))
  
  # if(!("stat" %in% names(x))){
  #    # Theoretical distribution
  #  which_distribution(x, 
  #                     theory_type <- attr(x, "theory_type"),
  #                     obs_stat = obs_stat,
  #                     direction = direction) 
  # }
  
  return(pvalue)
}

simulation_based_p_value <- function(x, obs_stat, direction){
  
  if(direction %in% c("less", "left")){
   p_value <- x %>% 
      dplyr::summarize(p_value = mean(stat <= obs_stat))
  }
  else if(direction %in% c("greater", "right")){
   p_value <- x %>% 
      dplyr::summarize(p_value = mean(stat >= obs_stat))
  }
  else{
    p_value <- x %>% two_sided_p_value(obs_stat = obs_stat)
  }
  
  p_value
}

two_sided_p_value <- function(x, obs_stat){
  
  if(stats::median(x$stat) >= obs_stat){
    basic_p_value <- get_percentile(x$stat, obs_stat) +
      (1 - get_percentile(x$stat, stats::median(x$stat) + 
                       stats::median(x$stat) - obs_stat))
  } else {
    basic_p_value <- 1 - get_percentile(x$stat, obs_stat) +
      (get_percentile(x$stat, stats::median(x$stat) + 
                            stats::median(x$stat) - obs_stat))
  }
  
  if(basic_p_value >= 1)
    # Catch all if adding both sides produces a number
    # larger than 1. Should update with test in that
    # scenario instead of using >=
    return(tibble::tibble(p_value = 1))
  else
    return(tibble::tibble(p_value = basic_p_value))
}

#' @export
#' @rdname get_pvalue
get_pvalue <- p_value

# which_distribution <- function(x, theory_type, obs_stat, direction){
#   
#   param <- attr(x, "distr_param")
#   if(!is.null(attr(x, "distr_param2")))
#     param2 <- attr(x, "distr_param2")
#   
#   if(theory_type == "Two sample t")
#     return(pt(q = obs_stat,
#                              df = param,
#                              lower.tail = set_lower_tail(direction))
#          )
# 
# }

#theory_t_pvalue <- 
  
# set_lower_tail <- function(direction){
#   if(direction %in% c("greater", "right"))
#     lower_tail <- FALSE
#   else
#     lower_tail <- TRUE
#   
#   lower_tail
# }  
