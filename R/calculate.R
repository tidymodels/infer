#' Calculate summary statistics
#' @param x the output from \code{\link{generate}} for computation-based inference. Down the road,
#' \code{\link{hypothesize}} will also be able to be piped in to here for theory-based inference.
#' @param stat a string giving the type of the statistic to calculate. Current
#' options include "mean", "median", "sd", "prop", "diff in means",
#' "diff in medians", "diff in props", "Chisq", "F", and "slope".
#' @param order a string vector of specifying the order in which the levels of
#' the explanatory variable should be ordered for subtraction, where
#' \code{order = c("first", "second")} means \code{("first" - "second")}
#' Needed for inference on difference in means, medians, or proportions.
#' @param ... to pass options like \code{na.rm = TRUE} into functions like mean, sd, etc.
#' @return A tibble containing a \code{stat} column of calculated statistics
#' @importFrom dplyr group_by summarize
#' @importFrom rlang !! sym quo enquo eval_tidy UQ
#' @export
#' @examples
#' # Permutation test for two binary variables
#'   mtcars %>%
#'     dplyr::mutate(am = factor(am), vs = factor(vs)) %>%
#'     specify(am ~ vs, success = "1") %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "diff in props", order = c("1", "0"))

calculate <- function(x, stat, order = NULL, ...) {
  
  # TODO: Check to see if dplyr::group_by(replicate) is needed since
  # generate() does a grouping of replicate
  
  assertive::assert_is_tbl(x)
  assertive::assert_is_a_string(stat)
  
  if (is.null(attr(x, "response"))){
    stop(paste("The response variable is not set.",
               "Make sure to `specify()` it first."))
  }
  
  if (!stat %in% c("mean", "median", "sd", "prop",
                   "diff in means", "diff in medians", "diff in props",
                   "Chisq", "F", "slope")){
    stop(paste("You specified a string for `stat` that is not implemented.",
               "Check your spelling and `?calculate` for current options."))
  }
  
  if (stat %in% c("mean", "median", "sd")){
    col <- setdiff(names(x), "replicate")
    
    if (!is.numeric(x[[col]])){
      stop(paste0("Calculating a ",
                  stat,
                  " here is not appropriate \n  since the `",
                  col,
                  "` variable is not numeric."))
    }
  }
  
  if (stat == "mean") {
    df_out <- x %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = mean(UQ(sym(col)), ...))
  }
  
  if (stat == "median") {
    df_out <- x %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = stats::median(UQ(sym(col)), ...))
  }
  
  if (stat == "sd") {
    df_out <- x %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = stats::sd(UQ(sym(col)), ...))
  }
  
  if (stat == "prop") {
    col <- attr(x, "response")
    
    if(!is.factor(x[[col]])){
      stop(paste0("Calculating a ",
                  stat,
                  " here is not appropriate \n  since the `",
                  col,
                  "` variable is not a factor."))
    }
    
    success <- attr(x, "success")
    df_out <- x %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = mean(rlang::eval_tidy(col) == 
                                     rlang::eval_tidy(success), 
                                   ...))
  }
  
  if (stat %in% c("diff in means", "diff in medians", "diff in props", "F")){
    if (!is.factor(x[[as.character(attr(x, "explanatory"))]])){
      stop(paste0("The explanatory variable of `",
                  attr(x, "explanatory"),
                  "` is not appropriate \n  since '",
                  stat,
                  "' is expecting the explanatory variable to be a factor."))
    }
  }
  
  if (stat %in% c("F", "slope", "diff in means", "diff in medians")){
    if (!is.null(attr(x, "explanatory"))
        & !is.numeric(x[[as.character(attr(x, "response"))]])){
      stop(paste0("The response variable of `",
                  attr(x, "response"),
                  "` is not appropriate \n  since '",
                  stat,
                  "' is expecting the response variable to be numeric."))
    }
  }
  
  if (stat %in% c("diff in means", "diff in medians", "diff in props")){
    if (length(unique(x[[as.character(attr(x, "explanatory"))]])) != 2){
      stop(paste("Statistic is a difference, the explanatory variable",
                 "should have two levels."))
    }
    if (is.null(order)){
      stop(paste("Statistic is a difference, specify the `order`` in",
                 "which to subtract. Check `?calculate` for details."))
    }
    if (!is.null(order) & xor(is.na(order[1]), is.na(order[2]))){
      stop(paste("Only one level specified in `order`.",
                 "Both levels need to be specified."))
    }
    if (!is.null(order) & length(order) > 2){
      stop("`order` is expecting only two entries.")
    }
    if (!is.null(order) & 
        (order[1] %in% 
         unique(x[[as.character(attr(x, "explanatory"))]]) == FALSE)){
      stop(paste(order[1], "is not a level of the explanatory variable."))
    }
    if (!is.null(order) & 
        (order[2] %in% 
         unique(x[[as.character(attr(x, "explanatory"))]]) == FALSE)){
      stop(paste(order[2], "is not a level of the explanatory variable."))
    }
  }
  
  if (stat == "diff in means") {
    df_out <- x %>%
      dplyr::group_by(replicate, UQ(attr(x, "explanatory"))) %>%
      dplyr::summarize(xbar = mean(UQ(attr(x, "response"), ...))) %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = xbar[UQ(attr(x, "explanatory")) == order[1]] 
                       - xbar[UQ(attr(x, "explanatory")) == order[2]])
  }
  
  if (stat == "diff in medians") {
    df_out <- x %>%
      dplyr::group_by(replicate, UQ(attr(x, "explanatory"))) %>%
      dplyr::summarize(xtilde = 
                         stats::median(UQ(attr(x, "response"), ...))) %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = xtilde[UQ(attr(x, "explanatory")) == order[1]] 
                       - xtilde[UQ(attr(x, "explanatory")) == order[2]])
  }
  
  if (stat %in% c("diff in props", "Chisq")){
    if (!is.null(attr(x, "explanatory")) & 
        !is.factor(x[[as.character(attr(x, "response"))]])){
      stop(paste0("The response variable of `",
                  attr(x, "response"),
                  "` is not appropriate \n  since '",
                  stat,
                  "' is expecting the response variable to be a factor."))
    }
  }
  
  if (stat == "diff in props") {

    # Detected in another test    
#    if (length(levels(x[[as.character(attr(x, "explanatory"))]])) != 2){
#      stop(paste0("The explanatory variable of `",
#                  attr(x, "explanatory"),
#                  "` does not have exactly two levels. \n",
#                  "Convert it to have only two levels and try again."))
#    }
    
    col <- attr(x, "response")
    success <- attr(x, "success")
    df_out <- x %>%
      dplyr::group_by(replicate, UQ(attr(x, "explanatory"))) %>%
      dplyr::summarize(prop = mean(rlang::eval_tidy(col) == 
                                     rlang::eval_tidy(success), ...)) %>%
      dplyr::summarize(stat = prop[UQ(attr(x, "explanatory")) == order[1]]
                       - prop[UQ(attr(x, "explanatory")) == order[2]])
  }
  
  if (stat == "Chisq") {
    ## The following could stand to be cleaned up
    n   <- attr(x, "biggest_group_size")
    
    if (is.null(attr(x, "explanatory"))) {
      expected <- n * attr(x, "params")
      df_out <- x %>%
        dplyr::summarize(stat = sum((table(UQ(attr(x, "response"))) 
                                     - expected)^2 / expected, ...))
    } else {
      # This is not matching with chisq.test
      # obs_tab <- x %>%
      #   dplyr::filter(replicate == 1) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::select(!!attr(x, "response"), 
      #                 UQ(attr(x, "explanatory"))) %>%
      #   table()
      # expected <- outer(rowSums(obs_tab), colSums(obs_tab)) / n
      # df_out <- x %>%
      #   dplyr::summarize(stat = sum((table(UQ(attr(x, "response")), 
      #                                      UQ(attr(x, "explanatory")))
      #                                - expected)^2 / expected, ...))
      
      df_out <- x %>%
        dplyr::group_by(replicate) %>%
        dplyr::do(broom::tidy(stats::chisq.test(
          table(.[[as.character(attr(x, "response"))]],
                .[[as.character(attr(x, "explanatory"))]])))) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(replicate = as.factor(replicate), stat = statistic)
    }
  }
  
  if (stat == "F") {
    df_out <- x %>%
      dplyr::summarize(stat = stats::anova(
        stats::lm(UQ(attr(x, "response")) ~ UQ(attr(x, "explanatory")))
      )$`F value`[1])
  }
  
  if (stat == "slope") {
    df_out <- x %>%
      dplyr::summarize(stat = stats::coef(
        stats::lm(UQ(attr(x, "response")) ~ UQ(attr(x, "explanatory"))))[2])
  }
  
  return(df_out)
}
