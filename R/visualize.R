#' (Currently) Visualize the randomization-based distribution 
#' (To be updated to include theory-based distributions)
#' @param data the output from \code{\link{calculate}}
#' @param bins the number of bins in the histogram
#' @param method a string giving the method to display. Options are 
#' "randomization", "theoretical", or "both"
#' with "both" corresponding to "randomization" and "theoretical"
#' @param dens_color a character or hex string specifying the color of the
#'  theoretical density curve
#' @param obs_stat a numeric value corresponding to what the observed 
#' statistic is
#' @param obs_stat_color a character or hex string specifying the color of
#'  the observed statistic
#' @param direction a string specifying in which direction the shading 
#' should occur. Options are "less", "greater", or "two_sided".
#' Can also specify "left", "right", or "both".
#' @param ... currently ignored
#' @importFrom ggplot2 ggplot geom_histogram aes stat_function ggtitle xlab ylab 
#' @importFrom ggplot2 geom_vline geom_rect
#' @importFrom stats dt qt df qf dnorm qnorm dchisq qchisq
#' @return A ggplot object showing the randomization-based distribution as a
#'  histogram or bar graph. Also used to show the theoretical curves.
#' @export
#' @examples 
#' # Permutations to create randomization null distribution for 
#' # one numerical response and one categorical predictor
#' # using t statistic
#' mtcars %>%
#'     dplyr::mutate(am = factor(am)) %>%
#'     specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "t", order = c("1", "0")) %>%
#'     visualize(method = "randomization") #default method
#' 
#' # Theoretical t distribution for 
#' # one numerical response and one categorical predictor
#' # using t statistic
#' mtcars %>%
#'     dplyr::mutate(am = factor(am)) %>%
#'     specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'     hypothesize(null = "independence") %>%
#'     # generate() is not needed since we are not doing randomization
#'     # calculate(stat = "t") ## Not needed since t implied based on variable types
#'     visualize(method = "theoretical") #default method
#' 
#' # Overlay theoretical distribution on top of randomized t-statistics
#' mtcars %>%
#'     dplyr::mutate(am = factor(am)) %>%
#'     specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "t", order = c("1", "0")) %>%
#'     visualize(method = "both")

visualize <- function(data, bins = 30, method = "randomization", 
                      dens_color = "black",
                      obs_stat = NULL, 
                      obs_stat_color = "#00BFC4",
                      direction = NULL, ...) {
  
  assertive::assert_is_data.frame(data)
  assertive::assert_is_numeric(bins)
  
  if(!is.null(direction) & is.null(obs_stat))
    stop("Shading requires observed statistic value to be given.")
  
  if(method == "randomization"){
    # TODO:  determine whether a bar graph or a histogram is
    # more appropriate
    if(is.null(direction)){
      infer_plot <- ggplot(data = data, mapping = aes(x = stat)) +
        geom_histogram(bins = bins, color = "white")
    } else {
      infer_plot <- shade_density_check(data = data, #gg_plot = infer_t_plot,
                                        obs_stat = obs_stat,
                                        direction = direction,
                                        bins = bins,
                                        density = FALSE)
  }
  
  # if(!is.null(direction)){
  #   if(direction %in% c("less", "left")){
  #     infer_plot <- ggplot(data = data, mapping = aes(x = stat)) +
  #       geom_histogram(bins = bins, color = "white", 
  #                      mapping = aes(fill = (stat <= obs_stat)))
  #   }
  #   if(direction %in% c("greater", "right")){
  #     infer_plot <- ggplot(data = data, mapping = aes(x = stat)) +
  #       geom_histogram(bins = bins, color = "white", 
  #                      mapping = aes(fill = (stat >= obs_stat)))
  #   }
  #   if(direction %in% c("two_sided", "both") & obs_stat >= 0){
  #     infer_plot <- ggplot(data = data, mapping = aes(x = stat)) +
  #       geom_histogram(bins = bins, color = "white", 
  #                      mapping = aes(fill = (abs(stat) >= obs_stat)))
  #   }
  #   if(direction %in% c("two_sided", "both") & obs_stat < 0){
  #     infer_plot <- ggplot(data = data, mapping = aes(x = stat)) +
  #       geom_histogram(bins = bins, color = "white", 
  #                      mapping = aes(fill = (abs(stat) <= obs_stat)))
  #   }
  # }
  
} else if(method == "theoretical"){
  
#  print(attr(data, "theory_type"))
  
  if(attr(data, "theory_type") == "Two sample t"){    
    infer_plot <- theory_t_plot(deg_freedom = attr(data, "distr_param"),
                                statistic_text = "t",
                                dens_color = dens_color)
  }
  
  else if(attr(data, "theory_type") == "ANOVA"){
    infer_plot <- theory_anova_plot(
      deg_freedom_top = attr(data, "distr_param"), 
      deg_freedom_bottom = attr(data, "distr_param2"), 
      statistic_text = "F",
      dens_color = dens_color)
  }
  
  else if(attr(data, "theory_type") %in% 
          c("One sample prop z", "Two sample props z")){
    infer_plot <- theory_z_plot(statistic_text = "z")
  }
  
  else if(attr(data, "theory_type") == "Chi-square test of indep"){    
    infer_plot <- theory_chisq_plot(deg_freedom = attr(data, "distr_param"),
                                statistic_text = "Chi-Square",
                                dens_color = dens_color)
  }
  
  else
    stop(paste0("'", attr(data, "theory_type"), "' is not implemented yet."))
  
} else { #method == "both"
  
 # print(attr(data, "theory_type"))
  
  if(attr(data, "theory_type") == "Two sample t"){
    infer_plot <- both_t_plot(data = data, 
                              deg_freedom = attr(data, "distr_param"),
                              statistic_text = "t",
                              dens_color = dens_color,
                              bins = bins,
                              direction = direction,
                              obs_stat = obs_stat)
  }
  
  else if(attr(data, "theory_type") == "ANOVA"){
    infer_plot <- both_anova_plot(
      data = data, 
      deg_freedom_top = attr(data, "distr_param"), 
      deg_freedom_bottom = attr(data, "distr_param2"), 
      statistic_text = "F", 
      dens_color = dens_color,
      bins = bins,
      direction = direction,
      obs_stat = obs_stat) 
  }
  
  else if(attr(data, "theory_type") %in% 
          c("One sample prop z", "Two sample props z")){
    infer_plot <- both_z_plot(data = data, 
                              statistic_text = "z", 
                              dens_color = dens_color,
                              bins = bins,
                              direction = direction,
                              obs_stat = obs_stat) 
  }
 
  else if(attr(data, "theory_type") == "Chi-square test of indep"){
    infer_plot <- both_chisq_plot(data = data, 
                                  deg_freedom = attr(data, "distr_param"),
                                  statistic_text = "Chi-Square", 
                                  dens_color = dens_color,
                                  bins = bins,
                                  direction = "right",
                                  obs_stat = obs_stat) 
  }
   
  else
    stop(paste0("'", attr(data, "theory_type"), "' is not implemented yet."))
  
}

if(!is.null(obs_stat) & !is.null(direction)){
#  if(!(direction %in% c("both", "two_sided"))){
    infer_plot <- infer_plot +
      geom_vline(xintercept = obs_stat, size = 2, color = obs_stat_color)
#  }
}

# if(!is.null(obs_stat) & !is.null(direction)){
#   if(direction %in% c("both", "two_sided")){
#     infer_plot <- infer_plot +
#       geom_vline(xintercept = obs_stat, size = 2, color = obs_stat_color)
#   }
# }

infer_plot
}

theory_t_plot <- function(deg_freedom, statistic_text = "t", 
                          dens_color = "black", ...){
  ggplot(data.frame(x = c(qt(0.001, deg_freedom), 
                          qt(0.999, deg_freedom))), 
         aes(x)) + 
    stat_function(fun = dt, args = list(df = deg_freedom), 
                  color = dens_color) +
    ggtitle(paste("Theoretical", statistic_text, 
                  "Null Distribution")) +
    xlab("") +
    ylab("")
}

both_t_plot <- function(data, deg_freedom, statistic_text = "t",
                        dens_color = "black",
                        obs_stat = NULL,
                        direction = NULL, bins = 30,...){
  #  infer_t_plot <- ggplot(data = data, mapping = aes(x = stat))
  
  infer_t_plot <- shade_density_check(data = data, #gg_plot = infer_t_plot,
                                      obs_stat = obs_stat,
                                      direction = direction,
                                      bins = bins)
  
  infer_t_plot +
    stat_function(fun = dt, args = list(df = deg_freedom), 
                  color = dens_color) +
    ggtitle(paste("Randomization-Based and Theoretical", 
                  statistic_text, "Null Distributions")) +
    xlab("tstat") +
    ylab("")
}

theory_anova_plot <- function(deg_freedom_top, deg_freedom_bottom, 
                              statistic_text = "F", dens_color = "black", ...){
  ggplot(data.frame(x = c(qf(0.001, deg_freedom_top, deg_freedom_bottom), 
                          qf(0.999, deg_freedom_top, deg_freedom_bottom))), 
         aes(x)) + 
    stat_function(fun = df, 
                  args = list(df1 = deg_freedom_top, df2 = deg_freedom_bottom),
                  color = dens_color) +
    ggtitle(paste("Theoretical", statistic_text, "Null Distribution")) +
    xlab("") +
    ylab("")
}

both_anova_plot <- function(data, deg_freedom_top, 
                            deg_freedom_bottom, statistic_text = "F",
                            dens_color = "black",
                            obs_stat = NULL,
                            direction = NULL, bins = 30,....){
  
  infer_anova_plot <- shade_density_check(data = data, 
                                          obs_stat = obs_stat,
                                          direction = direction,
                                          bins = bins)
  
  infer_anova_plot <- infer_anova_plot +
    stat_function(fun = df, 
                  args = list(df1 = deg_freedom_top, df2 = deg_freedom_bottom),
                  color = dens_color) +
    ggtitle(paste("Randomization-Based and Theoretical", 
                  statistic_text, "Null Distributions")) +
    xlab("Fstat") +
    ylab("")  
}

theory_z_plot <- function(statistic_text = "z", dens_color = "black", ...){
  ggplot(data.frame(x = c(qnorm(0.001), qnorm(0.999))), aes(x)) + 
    stat_function(fun = dnorm, color = dens_color) +
    ggtitle(paste("Theoretical", statistic_text, "Null Distribution")) +
    xlab("") +
    ylab("")
}

both_z_plot <- function(data, statistic_text = "z",
                        dens_color = "black",
                        obs_stat = NULL,
                        direction = NULL, bins = 30,...){
  infer_z_plot <- shade_density_check(data = data, 
                                      obs_stat = obs_stat,
                                      direction = direction,
                                      bins = bins)
  
  infer_z_plot +
    stat_function(fun = dnorm, color = dens_color) +
    ggtitle(paste("Randomization-Based and Theoretical", 
                  statistic_text, "Null Distributions")) +
    xlab("zstat") +
    ylab("")
}

theory_chisq_plot <- function(deg_freedom, statistic_text = "Chi-Square", 
                              dens_color = "black", ...){
  ggplot(data.frame(x = c(qchisq(0.001, deg_freedom), 
                          qchisq(0.999, deg_freedom))), 
         aes(x)) + 
    stat_function(fun = dchisq, args = list(df = deg_freedom), 
                  color = dens_color) +
    ggtitle(paste("Theoretical", statistic_text, "Null Distribution")) +
    xlab("") +
    ylab("")
}

both_chisq_plot <- function(data, deg_freedom, statistic_text = "Chi-Square",
                        dens_color = "black",
                        obs_stat = NULL,
                        direction = "greater", bins = 30,...){

  infer_chisq_plot <- shade_density_check(data = data,
                                      obs_stat = obs_stat,
                                      direction = direction,
                                      bins = bins)
  
  infer_chisq_plot +
    stat_function(fun = dchisq, args = list(df = deg_freedom), 
                  color = dens_color) +
    ggtitle(paste("Randomization-Based and Theoretical", 
                  statistic_text, "Null Distributions")) +
    xlab("chisqstat") +
    ylab("")
}


shade_density_check <- function(data = data, #gg_plot, 
                                obs_stat, direction, bins, 
                                density = TRUE, 
                                ...){ 
  
  if(is.null(direction) | is.null(obs_stat)){
    if(density){
      gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
        geom_histogram(bins = bins, color = "white",
                       mapping = aes(y = ..density..))
    } else {
      gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
        geom_histogram(bins = bins, color = "white")
    }
  }
  
  if(!is.null(obs_stat)){
    if(!is.null(direction)){
      if(density){
        gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
          geom_histogram(bins = bins, color = "white",
                         mapping = aes(y = ..density..))
      } else {
        gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
          geom_histogram(bins = bins, color = "white")
      }
      
      if(direction %in% c("less", "left")){
        gg_plot <- gg_plot +
          geom_rect(fill = "lightcyan", alpha = 0.02, 
                    aes(xmin = -Inf, xmax = obs_stat, ymin = 0, ymax = Inf))
      }
      if(direction %in% c("greater", "right")){
        gg_plot <- gg_plot +
          geom_rect(fill = "lightcyan", alpha = 0.02, 
                    aes(xmin = obs_stat, xmax = Inf, ymin = 0, ymax = Inf))
      }
      if(direction %in% c("two_sided", "both") & obs_stat >= 0){
        gg_plot <- gg_plot +
          geom_rect(fill = "lightcyan", alpha = 0.02, 
                    aes(xmin = obs_stat, xmax = Inf, ymin = 0, ymax = Inf)) +
          geom_rect(fill = "lightcyan", alpha = 0.02, 
                    aes(xmin = -Inf, xmax = -obs_stat, ymin = 0, ymax = Inf)) 
      }
      if(direction %in% c("two_sided", "both") & obs_stat < 0){
        gg_plot <- gg_plot +
          geom_rect(fill = "lightcyan", alpha = 0.02, 
                    aes(xmin = -Inf, xmax = obs_stat, ymin = 0, ymax = Inf)) +
          geom_rect(fill = "lightcyan", alpha = 0.02, 
                    aes(xmin = -obs_stat, xmax = Inf, ymin = 0, ymax = Inf)) 
      }
    }
    
    # if(!is.null(obs_stat)){
    #   if(!is.null(direction)){
    #     # Thanks to Jim Hester for the hint on getting this to work!
    #     gg_plot <- bin_vector(data$stat,
    #                           bin_breaks_bins(range(data$stat), bins)) %>%
    #       ggplot()
    #     
    #     if(direction %in% c("less", "left")){
    #       gg_plot <- gg_plot + 
    #         geom_rect(color = "white", aes(xmin = xmin, xmax = xmax, ymin = 0, 
    #                                        ymax = density, 
    #                                        fill = x <= obs_stat))
    #     }
    #     if(direction %in% c("greater", "right")){
    #       gg_plot <- gg_plot + 
    #         geom_rect(color = "white", aes(xmin = xmin, xmax = xmax, ymin = 0, 
    #                                        ymax = density, 
    #                                        fill = x >= obs_stat))
    #     }
    #     if(direction %in% c("two_sided", "both") & obs_stat >= 0){
    #       gg_plot <- gg_plot + 
    #         geom_rect(color = "white", aes(xmin = xmin, xmax = xmax, ymin = 0, 
    #                                        ymax = density, 
    #                                        fill = abs(x) >= obs_stat))
    #     }
    #     if(direction %in% c("two_sided", "both") & obs_stat < 0){
    #       gg_plot <- gg_plot + 
    #         geom_rect(color = "white", aes(xmin = xmin, xmax = xmax, ymin = 0, 
    #                                        ymax = density, 
    #                                        fill = abs(x) >= abs(obs_stat)))
    #     }
    #   }
    # }
  }
  gg_plot
}
