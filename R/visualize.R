#' Visualize the distribution of the simulation-based inferential statistics
#' or the theoretical distribution (or both!)
#' @param data the output from \code{\link{calculate}}
#' @param bins the number of bins in the histogram
#' @param method a string giving the method to display. Options are 
#' "simulation", "theoretical", or "both"
#' with "both" corresponding to "simulation" and "theoretical"
#' @param dens_color a character or hex string specifying the color of the
#'  theoretical density curve
#' @param obs_stat a numeric value or 1x1 data frame corresponding to what the observed 
#' statistic is
#' @param obs_stat_color a character or hex string specifying the color of
#'  the observed statistic
#' @param shade_color a character or hex string specifying the color to shade
#' @param direction a string specifying in which direction the shading 
#' should occur. Options are "less", "greater", or "two_sided".
#' Can also specify "left", "right", or "both".
#' @param ... currently ignored
#' @importFrom ggplot2 ggplot geom_histogram aes stat_function ggtitle  
#' @importFrom ggplot2 xlab ylab geom_vline geom_rect geom_bar
#' @importFrom stats dt qt df qf dnorm qnorm dchisq qchisq
#' @return A ggplot object showing the simulation-based distribution as a
#'  histogram or bar graph. Also used to show the theoretical curves.
#' @export
#' @examples 
#' # Permutations to create a simulation-based null distribution for 
#' # one numerical response and one categorical predictor
#' # using t statistic
#' mtcars %>%
#'     dplyr::mutate(am = factor(am)) %>%
#'     specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "t", order = c("1", "0")) %>%
#'     visualize(method = "simulation") #default method
#' 
#' # Theoretical t distribution for 
#' # one numerical response and one categorical predictor
#' # using t statistic
#' mtcars %>%
#'     dplyr::mutate(am = factor(am)) %>%
#'     specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'     hypothesize(null = "independence") %>%
#'     # generate() is not needed since we are not doing simulation
#'     calculate(stat = "t", order = c("1", "0")) %>%
#'     visualize(method = "theoretical")
#' 
#' # Overlay theoretical distribution on top of randomized t-statistics
#' mtcars %>%
#'     dplyr::mutate(am = factor(am)) %>%
#'     specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "t", order = c("1", "0")) %>%
#'     visualize(method = "both")

visualize <- function(data, bins = 15, method = "simulation", 
                      dens_color = "black",
                      obs_stat = NULL, 
                      obs_stat_color = "#e51010",#"#00BFC4",
                      shade_color = "#efb8b8",
                      direction = NULL, ...) {
  
  assertive::assert_is_data.frame(data)
  assertive::assert_is_numeric(bins)
  assertive::assert_is_character(method)
  
  if(!is.null(obs_stat)){
    if("data.frame" %in% class(obs_stat)){
      assertive::assert_is_data.frame(obs_stat)
      # [[1]] is used in case `stat` is not specified as name of 1x1
      obs_stat <- obs_stat[[1]]
    }
    else{
      assertive::assert_is_numeric(obs_stat)
    }
  }
  assertive::assert_is_character(dens_color)
  assertive::assert_is_character(obs_stat_color)
  assertive::assert_is_character(shade_color)
  if(!is.null(direction))
    assertive::assert_is_character(direction)
  
  if(!is.null(direction) && is.null(obs_stat))
    stop("Shading requires observed statistic `obs_stat` value to be given.")
  
  if(method == "simulation"){
    
    infer_plot <- visualize_simulation(data = data, bins = bins, 
                                          dens_color = dens_color,
                                          obs_stat = obs_stat, 
                                          obs_stat_color = obs_stat_color,
                                          direction = direction, 
                                          shade_color = shade_color, ...)
    
  } else if(method == "theoretical"){
    
    infer_plot <- visualize_theoretical(data = data,
                                        dens_color = dens_color,
                                        obs_stat = obs_stat, 
                                        obs_stat_color = obs_stat_color,
                                        direction = direction, 
                                        shade_color = shade_color, ...)
    
    
  } else if(method == "both"){
    
    if(!("stat" %in% names(data)))
      stop(paste0('`generate()` and `calculate()` are both required ', 
                  'to be done prior to `visualize(method = "both")`'))
    
    if(length(unique(data$replicate)) < 100)
      warning(paste("With only", length(unique(data$stat)),
                    "replicates, it may be difficult to see the",
                    "relationship between simulation and theory."))
    
    infer_plot <- visualize_both(data = data, bins = bins, 
                                 dens_color = dens_color,
                                 obs_stat = obs_stat, 
                                 obs_stat_color = obs_stat_color,
                                 direction = direction,
                                 shade_color = shade_color, ...)
  } else {
    stop(paste("Provide `method` with one of three options:",
               "`theoretical`, `both`, or `simulation`",
               "`simulation` is the default.")
    )
  }
  
  if(!is.null(obs_stat)){#&& !is.null(direction)
    infer_plot <- infer_plot +
      geom_vline(xintercept = obs_stat, size = 2, color = obs_stat_color, ...)
  }
  
  infer_plot
}


theory_t_plot <- function(deg_freedom, statistic_text = "t", 
                          dens_color = "black", ...){
  ggplot(data.frame(x = c(qt(0.001, deg_freedom), 
                          qt(0.999, deg_freedom)))) + 
    stat_function(mapping = aes(x), fun = dt, args = list(df = deg_freedom), 
                  color = dens_color) +
    ggtitle(paste("Theoretical", statistic_text, 
                  "Null Distribution")) +
    xlab("") +
    ylab("")
}

both_t_plot <- function(data = data, deg_freedom, statistic_text = "t",
                        dens_color = "black",
                        obs_stat = NULL,
                        direction = NULL, bins = 15,...){
  
  infer_t_plot <- shade_density_check(data = data,
                                      obs_stat = obs_stat,
                                      direction = direction,
                                      bins = bins)

  infer_t_plot +
    stat_function(fun = dt, args = list(df = deg_freedom), 
                  color = dens_color) +
    ggtitle(paste("Simulation-Based and Theoretical", 
                  statistic_text, "Null Distributions")) +
    xlab("tstat") +
    ylab("")
}

theory_anova_plot <- function(deg_freedom_top, deg_freedom_bottom, 
                              statistic_text = "F", dens_color = "black", ...){
  ggplot(data.frame(x = c(qf(0.001, deg_freedom_top, deg_freedom_bottom), 
                          qf(0.999, deg_freedom_top, deg_freedom_bottom)))) + 
    stat_function(mapping = aes(x), fun = df, 
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
                            direction = NULL, bins = 15,....){
  
  if(!is.null(direction) && !(direction %in% c("greater", "right")))
    warning(paste("F usually corresponds to right-tailed tests. Proceed",
                  "with caution."))
  
  infer_anova_plot <- shade_density_check(data = data, 
                                          obs_stat = obs_stat,
                                          direction = direction,
                                          bins = bins)
  
  infer_anova_plot <- infer_anova_plot +
    stat_function(fun = df, 
                  args = list(df1 = deg_freedom_top, df2 = deg_freedom_bottom),
                  color = dens_color) +
    ggtitle(paste("Simulation-Based and Theoretical", 
                  statistic_text, "Null Distributions")) +
    xlab("Fstat") +
    ylab("")  
}

theory_z_plot <- function(statistic_text = "z", dens_color = "black",  ...){
  ggplot(data.frame(x = c(qnorm(0.001), qnorm(0.999)))) + 
    stat_function(mapping = aes(x), fun = dnorm, color = dens_color) +
    ggtitle(paste("Theoretical", statistic_text, "Null Distribution")) +
    xlab("") +
    ylab("")
}

both_z_plot <- function(data, statistic_text = "z",
                        dens_color = "black",
                        obs_stat = NULL,
                        direction = NULL, bins = 15,...){
  infer_z_plot <- shade_density_check(data = data, 
                                      obs_stat = obs_stat,
                                      direction = direction,
                                      bins = bins)
  
  infer_z_plot +
    stat_function(fun = dnorm, color = dens_color) +
    ggtitle(paste("Simulation-Based and Theoretical", 
                  statistic_text, "Null Distributions")) +
    xlab("zstat") +
    ylab("")
}

theory_chisq_plot <- function(deg_freedom, 
                              statistic_text = "Chi-Square", 
                              dens_color = "black", ...){
  ggplot(data.frame(x = c(qchisq(0.001, deg_freedom), 
                          qchisq(0.999, deg_freedom)))) + 
    stat_function(mapping = aes(x), fun = dchisq, 
                  args = list(df = deg_freedom), 
                  color = dens_color) +
    ggtitle(paste("Theoretical", statistic_text, "Null Distribution")) +
    xlab("") +
    ylab("")
}

both_chisq_plot <- function(data, deg_freedom, statistic_text = "Chi-Square",
                            dens_color = "black",
                            obs_stat = NULL,
                            direction = "greater", bins = 15,...){
  
  if(!is.null(direction) && !(direction %in% c("greater", "right")))
     warning(paste("Chi-square usually corresponds to right-tailed tests.",
                   "Proceed with caution."))
  
  infer_chisq_plot <- shade_density_check(data = data,
                                          obs_stat = obs_stat,
                                          direction = direction,
                                          bins = bins)
  
  infer_chisq_plot +
    stat_function(fun = dchisq, args = list(df = deg_freedom), 
                  color = dens_color) +
    ggtitle(paste("Simulation-Based and Theoretical", 
                  statistic_text, "Null Distributions")) +
    xlab("chisqstat") +
    ylab("")
}


shade_density_check <- function(data = data,
                                obs_stat, 
                                direction, 
                                bins, 
                                density = TRUE, 
                                shade_color = "#efb8b8", ...) {
  
  if(is.null(direction) || is.null(obs_stat)){
    if(density){
      gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
        geom_histogram(bins = bins, color = "white",
                       mapping = aes(y = ..density..), ...)
    } #else {
      # Not sure if needed? Can't get tests to find it
      #gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
      #  geom_histogram(bins = bins, color = "white", ...)
    #}
  }
  
  if(!is.null(obs_stat)){
    if(!is.null(direction)){
      if(density){
        gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
          geom_histogram(bins = bins, color = "white",
                         mapping = aes(y = ..density..), ...)
      } else {
        gg_plot <- ggplot(data = data, mapping = aes(x = stat)) +
          geom_histogram(bins = bins, color = "white", ...)
      }
      
      if(direction %in% c("less", "left")){
        gg_plot <- gg_plot +
          geom_rect(fill = shade_color, alpha = 0.01, 
                    aes(xmin = -Inf, xmax = obs_stat, ymin = 0, ymax = Inf), 
                    ...)
      }
      if(direction %in% c("greater", "right")){
        gg_plot <- gg_plot +
          geom_rect(fill = shade_color, alpha = 0.01, 
                    aes(xmin = obs_stat, xmax = Inf, ymin = 0, ymax = Inf), 
                    ...)
      }
      
      if(direction %in% c("two_sided", "both") && 
         obs_stat >= stats::median(data$stat)){
        gg_plot <- gg_plot +
          geom_rect(fill = shade_color, alpha = 0.01,
                    mapping = aes(xmin = obs_stat, xmax = Inf, ymin = 0, 
                                  ymax = Inf), ...) +
          geom_rect(fill = shade_color, alpha = 0.01,
                    mapping = aes(
                      xmin = -Inf, 
                      xmax = stats::quantile(
                        data$stat, 
                        probs = 1 - get_percentile(data$stat, obs_stat)
                      ),
                      ymin = 0, ymax = Inf, ...)
          ) 
      }
      
      if(direction %in% c("two_sided", "both") && 
         obs_stat < stats::median(data$stat)){
        gg_plot <- gg_plot +
          geom_rect(fill = shade_color, alpha = 0.01,
                    mapping = aes(xmin = -Inf, xmax = obs_stat, ymin = 0, 
                                  ymax = Inf), ...) +
          geom_rect(fill = shade_color, alpha = 0.01,
                    mapping = aes( 
                      xmin = stats::quantile(
                        data$stat, 
                        probs = 1 - get_percentile(data$stat, obs_stat)
                      ), xmax = Inf, ymin = 0, ymax = Inf, ...)
          ) 
      }
    }
  }
    gg_plot
}

visualize_simulation <- function(data, bins = 15, method = "simulation", 
                                    dens_color = "black",
                                    obs_stat = NULL, 
                                    obs_stat_color = "#e51010",
                                    direction = NULL, 
                                    shade_color = "#efb8b8", ...) {
  if(is.null(direction)){
    if(length(unique(data$stat)) >= 10)
      infer_plot <- ggplot(data = data, mapping = aes(x = stat)) +
        geom_histogram(bins = bins, color = "white", ...)
    else
      infer_plot <- ggplot(data = data, mapping = aes(x = stat)) +
        geom_bar(...) +
        xlab("stat")
  } else {
    infer_plot <- shade_density_check(data = data,
                                        obs_stat = obs_stat,
                                        direction = direction,
                                        bins = bins,
                                        density = FALSE,
                                        shade_color = shade_color)
  }
  infer_plot
}

visualize_theoretical <- function(data,
                                  dens_color = "black",
                                  obs_stat = NULL, 
                                  obs_stat_color = "#e51010",#"#00BFC4",
                                  direction = NULL, 
                                  shade_color = "#efb8b8", ...) {
  
  warning(paste("Check to make sure the conditions", 
                "have been met for",
                "the theoretical method. `infer` currently does not check",
                "these for you."), call. = FALSE)
  
  if(!is.null(attr(data, "stat")) && 
     !(attr(data, "stat") %in% c("t", "z", "Chisq", "F")))
    warning(paste("Your `calculate`d statistic and the theoretical", 
                  "distribution are on different scales. Displaying only", 
                  "the theoretical distribution."))
  
  if(attr(data, "theory_type") %in% 
     c("Two sample t", "Slope with t", "One sample t")){    
    infer_plot <- theory_t_plot(deg_freedom = attr(data, "distr_param"),
                                statistic_text = "t",
                                dens_color = dens_color)
  }
  
  else if(attr(data, "theory_type") == "ANOVA"){
    
    if(!is.null(direction) && !(direction %in% c("greater", "right")))
      warning(paste("F usually corresponds to right-tailed tests. Proceed",
                    "with caution."))
    
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
  
  else if(attr(data, "theory_type") %in% 
          c("Chi-square test of indep", "Chi-square Goodness of Fit")){   
    
    if(!is.null(direction) && !(direction %in% c("greater", "right")))
      warning(paste("Chi-square usually corresponds to right-tailed tests.",
                    "Proceed with caution."))
    
    infer_plot <- theory_chisq_plot(deg_freedom = attr(data, "distr_param"),
                                    statistic_text = "Chi-Square",
                                    dens_color = dens_color)
  }
  
  else
    stop(paste0("'", attr(data, "theory_type"), "' is not implemented",
                "(possibly yet)."))
  
  # Move into its own function
  
  if(!is.null(obs_stat)){
    if(!is.null(direction)){
      if(direction %in% c("less", "left")){
        infer_plot <- infer_plot +
          geom_rect(data = data.frame(obs_stat), fill = shade_color, 
                    alpha = 0.6,
                    aes(xmin = -Inf, xmax = obs_stat, ymin = 0, ymax = Inf),
                    ...)
      }
      if(direction %in% c("greater", "right")){
        infer_plot <- infer_plot +
          geom_rect(data = data.frame(obs_stat), fill = shade_color, 
                    alpha = 0.6,
                    aes(xmin = obs_stat,
                        xmax = Inf, ymin = 0, ymax = Inf),
                    ...)
      }
      
      # Assuming two-tailed shading will only happen with theoretical 
      # distributions centered at 0
      if(direction %in% c("two_sided", "both") && obs_stat >= 0){
        infer_plot <- infer_plot +
          geom_rect(data = data.frame(obs_stat), fill = shade_color, 
                    alpha = 0.6,
                    aes(xmin = obs_stat, xmax = Inf, ymin = 0, ymax = Inf),
                    ...) +
          geom_rect(data = data.frame(obs_stat), fill = shade_color, 
                    alpha = 0.6,
                    aes(xmin = -Inf, xmax = -obs_stat, ymin = 0, ymax = Inf),
                    ...)
      }

      if(direction %in% c("two_sided", "both") && obs_stat < 0){
        infer_plot <- infer_plot +
          geom_rect(data = data.frame(obs_stat), fill = shade_color, 
                    alpha = 0.6,
                    aes(xmin = -Inf, xmax = obs_stat, ymin = 0, ymax = Inf),
                    ...) +
          geom_rect(data = data.frame(obs_stat), fill = shade_color, 
                    alpha = 0.6,
                    aes(xmin = -obs_stat, xmax = Inf, ymin = 0, ymax = Inf),
                    ...)
      }
    }
  }
  
  infer_plot
}

visualize_both <- function(data = data, bins = bins, 
                           dens_color = dens_color,
                           obs_stat = obs_stat, 
                           obs_stat_color = "#e51010",#"#00BFC4",
                           direction = direction, 
                           shade_color = "#efb8b8", ...) {
  
  warning(paste("Check to make sure the conditions", 
                "have been met for",
                "the theoretical method. `infer` currently does not check",
                "these for you."), call. = FALSE)
  
  if(!(attr(data, "stat") %in% c("t", "z", "Chisq", "F")))
    stop(paste("Your `calculate`d statistic and the theoretical distribution",
               "are on different scales. Use a standardized `stat` instead."))
  
  if(attr(data, "theory_type") %in% c("Two sample t", "Slope with t")){
    
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
  
  else if(
    attr(data, "theory_type") %in% 
    c("Chi-square test of indep", "Chi-square Goodness of Fit")){
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
  
  infer_plot
}

get_percentile <- function(vector, observation) {
  stats::ecdf(vector)(observation)
}
