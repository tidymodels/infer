#' Calculate summary statistics
#' @param x the output from \code{\link{generate}} for computation-based inference. Down the road,
#' \code{\link{hypothesize}} will also be able to be piped in to here for theory-based inference.
#' @param stat a string giving the type of the statistic to calculate. Current
#' options include "mean", "median", "sd", "prop", "diff in means",
#' "diff in medians", "diff in props", "Chisq", "F", and "slope".
#' @param ... to pass options like \code{na.rm = TRUE} into functions like mean, sd, etc.
#' @return A tibble containing a \code{stat} column of calculated statistics
#' @importFrom dplyr group_by summarize
#' @importFrom rlang !! sym quo enquo eval_tidy
#' @export
#' @examples
#'
#' # Permutation test for two binary variables
#'   mtcars %>%
#'     dplyr::mutate(am = factor(am), vs = factor(vs)) %>%
#'     specify(am ~ vs, success = "1") %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "diff in props")

calculate <- function(x, stat, ...) {
  
  # TODO: Check to see if dplyr::group_by(replicate) is needed since
  # generate() does a grouping of replicate
  
  assertive::assert_is_tbl(x)
  assertive::assert_is_a_string(stat)
  
  if(is.null(attr(x, "response"))){
    stop(paste("The response variable is not set as an attribute to the tibble.",
               "Make sure to run `specify()` to set it accordingly."))
  }
  
  if(!stat %in% c("mean", "median", "sd", "prop", 
                  "diff in means", "diff in medians", "diff in props",
                  "Chisq", "F", "slope")){
    stop(paste("You specified a string for `stat` that is not implemented.",
               "Check your spelling and `?calculate` for the current options."))
  }
  
  if(stat %in% c("mean", "median", "sd")){
    col <- setdiff(names(x), "replicate")
    
    if(!is.numeric(x[[col]])){
      stop(paste("Calculating a", 
                 stat,
                 "here is not appropriate \n  since the",
                 col,
                 "variable is not numeric."))
    }
  }
    
    if (stat == "mean") {
      df_out <- x %>%
        dplyr::group_by(replicate) %>%
        dplyr::summarize(stat = mean(!!sym(col), ...))
    }
    
    if (stat == "median") {
      df_out <- x %>%
        dplyr::group_by(replicate) %>%
        dplyr::summarize(stat = stats::median(!!sym(col), ...))
    }
    
    if (stat == "sd") {
      df_out <- x %>%
        dplyr::group_by(replicate) %>%
        dplyr::summarize(stat = stats::sd(!!sym(col), ...))
    }
    
    if (stat == "prop") {
      
      col <- attr(x, "response")
      
      if(!is.factor(x[[col]])){
        stop(paste("Calculating a", 
                   stat,
                   "here is not appropriate \n  since the",
                   col,
                   "variable is not a factor."))
      }
      
      success <- attr(x, "success")
      df_out <- x %>%
        dplyr::group_by(replicate) %>%
        dplyr::summarize(stat = mean(rlang::eval_tidy(col) == rlang::eval_tidy(success), ...))
    }
    
    if (stat == "diff in means") {
      df_out <- x %>%
        dplyr::group_by(replicate, !!attr(x, "explanatory")) %>%
        dplyr::summarize(xbar = mean(!!attr(x, "response"), ...)) %>%
        dplyr::group_by(replicate) %>%
        dplyr::summarize(stat = diff(xbar))
    }
    
    if (stat == "diff in medians") {
      df_out <- x %>%
        dplyr::group_by(replicate, !!attr(x, "explanatory")) %>%
        dplyr::summarize(xtilde = stats::median(!!attr(x, "response"), ...)) %>%
        dplyr::group_by(replicate) %>%
        dplyr::summarize(stat = diff(xtilde))
    }
    
    if (stat == "diff in props") {
      col <- attr(x, "response")
      success <- attr(x, "success")
      df_out <- x %>%
        dplyr::group_by(replicate, !!attr(x, "explanatory")) %>%
        dplyr::summarize(prop = mean(rlang::eval_tidy(col) == rlang::eval_tidy(success), ...)) %>%
        dplyr::summarize(stat = diff(prop))
    }
    
    if (stat == "Chisq") {
      ## The following could stand to be cleaned up
      n   <- attr(x, "biggest_group_size")
      
      if (is.null(attr(x, "explanatory"))) {
        expected <- n * attr(x, "params")
        df_out <- x %>%
          dplyr::summarize(stat = sum((table(!!attr(x, "response")) - expected)^2 / expected, ...))
      } else {
        obs_tab <- x %>%
          dplyr::filter(replicate == 1) %>%
          dplyr::ungroup() %>%
          dplyr::select(!!attr(x, "response"), !!attr(x, "explanatory")) %>%
          table()
        expected <- outer(rowSums(obs_tab), colSums(obs_tab)) / n
        df_out <- x %>%
          dplyr::summarize(stat = sum((table(!!attr(x, "response"), !!attr(x, "explanatory"))
                                       - expected)^2 / expected, ...))
        
      }
    }
    
    if (stat == "F") {
      df_out <- x %>%
        dplyr::summarize(stat = stats::anova(
          stats::lm(!! attr(x, "response") ~ !! attr(x, "explanatory"))
        )$`F value`[1])
    }
    
    if (stat == "slope") {
      df_out <- x %>%
        dplyr::summarize(stat = stats::coef(stats::lm(!! attr(x, "response") ~ !! attr(x, "explanatory")))[2])
    }
    
    return(df_out)
  }
  