#' (Currently) Visualize the resampling distribution 
#' (To be updated to include theory-based distributions)
#' @param data the output from \code{\link{calculate}}
#' @param bins the number of bins in the histogram
#' @param method a string giving the method to display. Options are "randomization", "theoretical", or "both"
#' @param ... currently ignored
#' @importFrom ggplot2 ggplot geom_histogram aes stat_function ggtitle xlab ylab
#' @importFrom stats dt qt
#' @export
#' @examples 
#' # Permutations to create randomization null distribution for 
#' # one numerical response and one categorical predictor
#' # using t statistic
#' if(require(dplyr)) {
#' mtcars %>%
#'     mutate(am = factor(am)) %>%
#'     specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "t") %>%
#'     visualize(method = "randomization") #default method
#' }
#' 
#' # Theoretical t distribution for 
#' # one numerical response and one categorical predictor
#' # using t statistic
#' if(require(dplyr)) {
#' mtcars %>%
#'     mutate(am = factor(am)) %>%
#'     specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'     hypothesize(null = "independence") %>%
#'     calculate(stat = "t") %>%
#'     visualize(method = "theoretical") #default method
#' }
#' 
#' # Overlay theoretical distribution on top of randomized t-statistics
#' if(require(dplyr)) {
#' mtcars %>%
#'     mutate(am = factor(am)) %>%
#'     specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "t") %>%
#'     visualize(method = "both")
#' }

visualize <- function(data, bins = 30, method = "randomization", ...) {
  if(method == "randomization"){
    # TODO:  determine whether a bar graph or a histogram is
    # more appropriate
    ggplot(data = data, mapping = aes(x = stat)) +
      geom_histogram(bins = bins, color = "white")
  } else if(method == "theoretical"){
    
    if (!is.null(attr(data, "response")) & 
         !is.null(attr(data, "explanatory")) & 
        attr(data, "response_type") %in% c("integer", "numeric") &
        attr(data, "explanatory_type") == "factor") {
      theory_t_plot(deg_freedom = attr(data, "distr_param"),
                    statistic_text = "t")
    }
    
  } else { #method == "both"
    if (!is.null(attr(data, "response")) & 
        !is.null(attr(data, "explanatory")) & 
        attr(data, "response_type") %in% c("integer", "numeric") &
        attr(data, "explanatory_type") == "factor") {
      both_t_plot(data = data, deg_freedom = attr(data, "distr_param"),
                    statistic_text = "t", bins = bins)
    }
  }
}

theory_t_plot <- function(deg_freedom, statistic_text, ...){
  ggplot(data.frame(x = c(qt(0.001, deg_freedom), qt(0.999, deg_freedom))), aes(x)) + 
    stat_function(fun = dt, args = list(df = deg_freedom), color = "blue") +
    ggtitle(paste("Theoretical", statistic_text, "Null Distribution")) +
    xlab("") +
    ylab("")
}

both_t_plot <- function(data, deg_freedom, statistic_text, bins = 30,...){
  ggplot(data = data, mapping = aes(x = stat)) +
    geom_histogram(aes(y = ..density..), color = "white", bins = bins) +
    stat_function(fun = dt, args = list(df = deg_freedom), color = "blue") +
    ggtitle(paste("Randomization-Based and Theoretical", statistic_text, "Null Distributions")) +
    xlab(paste0("tstat")) +
    ylab("")
}