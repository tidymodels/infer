#' Calculate summary statistics
#' @param x the output from \code{\link{generate}} for computation-based 
#' inference or the output from \code{\link{hypothesize}}
#' piped in to here for theory-based inference.
#' @param stat a string giving the type of the statistic to calculate. Current
#' options include "mean", "median", "sd", "prop", "diff in means",
#' "diff in medians", "diff in props", "Chisq", "F", "t", "z", and "slope".
#' @param order a string vector of specifying the order in which the levels of
#' the explanatory variable should be ordered for subtraction, where
#' \code{order = c("first", "second")} means \code{("first" - "second")}
#' Needed for inference on difference in means, medians, or proportions.
#' @param ... to pass options like \code{na.rm = TRUE} into functions like
#'mean, sd, etc.
#' @return A tibble containing a \code{stat} column of calculated statistics
#' @importFrom dplyr group_by summarize n
#' @importFrom rlang !! sym quo enquo eval_tidy
#' @export
#' @examples
#' # Permutation test for two binary variables
#'   mtcars %>%
#'     dplyr::mutate(am = factor(am), vs = factor(vs)) %>%
#'     specify(am ~ vs, success = "1") %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "diff in props", order = c("1", "0"))

calculate <- function(x, 
                      stat = c("mean", "median", "sd", "prop", "diff in means",
                             "diff in medians", "diff in props", "Chisq", "F",
                             "slope", "t", "z"),
                      order = NULL, ...) {
  
  assertive::assert_is_tbl(x)
  assertive::assert_is_a_string(stat)
  check_for_numeric_stat(x, stat)
  check_for_factor_stat(x, stat, explanatory_variable(x))
  check_args_and_attr(x, explanatory_variable(x), response_variable(x), stat)
  
  if(!has_response(x))
    stop(paste("The response variable is not set.",
               "Make sure to `specify()` it first."))
  
  if(is.null(attr(x, "generate")) || !attr(x, "generate")){
    if(is.null(attr(x, "null"))){
      x$replicate <- 1L
    }
    else if(stat %in% c("mean", "median", "sd", "prop",
                        "diff in means", "diff in medians", "diff in props",
                        "slope"))
      stop(paste0("Theoretical distributions do not exist (or have not been ", 
                  "implemented) for `stat = \"", stat, "\". Are you missing ",
                  "a `generate()` step?"))
    
    else
      # From `hypothesize()` to `calculate()`
      # Catch-all if generate was not called
      return(x)
  }

  if ( stat %in% c("diff in means", "diff in medians", "diff in props")  ||
       (!is.null(attr(x, "theory_type")) &&
      attr(x, "theory_type") %in% c("Two sample props z", "Two sample t") )) {
    
      check_order(x, explanatory_variable(x), order)
  }

  if (!(stat %in% c("diff in means", "diff in medians", "diff in props") ||
      (!is.null(attr(x, "theory_type")) &&
        attr(x, "theory_type") %in% c("Two sample props z", "Two sample t")))) {
    if (!is.null(order)){
      warning(paste("Statistic is not based on a difference;",
                    "the `order` argument",
                    "is ignored. Check `?calculate` for details."))
    }
  }

  # Use S3 method to match correct calculation
  result <- calc_impl(
    structure(stat, class = gsub(" ", "_", stat)), x, order, ...
  )
  
  if("NULL" %in% class(result))
      stop(paste0(
        "Your choice of `stat` is invalid for the ",
        "types of variables `specify`ed.")
      )
  else
    class(result) <- append("infer", class(result))
  
  attr(result, "response") <- attr(x, "response")
  attr(result, "success") <- attr(x, "success")
  attr(result, "explanatory") <- attr(x, "explanatory")
  attr(result, "response_type") <- attr(x, "response_type")
  attr(result, "explanatory_type") <- attr(x, "explanatory_type")
  attr(result, "distr_param") <- attr(x, "distr_param")
  attr(result, "distr_param2") <- attr(x, "distr_param2")
  attr(result, "theory_type") <- attr(x, "theory_type")
  attr(result, "stat") <- stat
  
  # For returning a 1x1 observed statistic value
  if(nrow(result) == 1)
    result <- select(result, stat)
  
  return(result)
}

calc_impl <- function(type, x, order, ...) UseMethod("calc_impl", type)


calc_impl.mean <- function(stat, x, order, ...) {
  col <- setdiff(names(x), "replicate")
  
  x %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarize(stat = mean(!!(sym(col)), ...))
  
}

calc_impl.median <- function(stat, x, order, ...) {
  col <- setdiff(names(x), "replicate")

  x %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarize(stat = stats::median(!!(sym(col)), ...))
}

calc_impl.sd <- function(stat, x, order, ...) {
  col <- setdiff(names(x), "replicate")

  x %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarize(stat = stats::sd(!!(sym(col)), ...))
}

calc_impl.prop <- function(stat, x, order, ...) {
  col <- setdiff(names(x), "replicate")
  
  if(!is.factor(x[[col]])){
    stop(paste0("Calculating a ",
                stat,
                " here is not appropriate since the `",
                col,
                "` variable is not a factor."))
  }

  success <- attr(x, "success")
  x %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarize(stat = mean(!!sym(col) == success,
      #rlang::eval_tidy(col) == rlang::eval_tidy(success),
                                 ...))
}


calc_impl.F <- function(stat, x, order, ...) {
  x %>%
    dplyr::summarize(stat = stats::anova(
      stats::lm(!!(attr(x, "response")) ~ !!(attr(x, "explanatory")))
    )$`F value`[1])
}



calc_impl.slope <- function(stat, x, order, ...) {
  x %>%
    dplyr::summarize(stat = stats::coef(
      stats::lm(!!(attr(x, "response")) ~ !!(attr(x, "explanatory"))))[2])
}

calc_impl.diff_in_means <- function(stat, x, order, ...) {
  x %>%
    dplyr::group_by(replicate, !!(attr(x, "explanatory"))) %>%
    dplyr::summarize(xbar = mean(!!attr(x, "response"), ...)) %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarize(stat = xbar[!!(attr(x, "explanatory")) == order[1]]
                     - xbar[!!(attr(x, "explanatory")) == order[2]])
}

calc_impl.diff_in_medians <- function(stat, x, order, ...) {
  x %>%
    dplyr::group_by(replicate, !!(attr(x, "explanatory"))) %>%
    dplyr::summarize(xtilde =
                       stats::median(!!attr(x, "response"), ...)) %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarize(stat = xtilde[!!(attr(x, "explanatory")) == order[1]]
                     - xtilde[!!(attr(x, "explanatory")) == order[2]])
}

calc_impl.Chisq <- function(stat, x, order, ...) {
  ## The following could stand to be cleaned up

  if (is.null(attr(x, "explanatory"))) {
    if(!is.null(attr(x, "params"))){
      # When `hypothesize()` has been called
      n <- attr(x, "biggest_group_size")
      expected <- n * attr(x, "params")
      x %>%
        dplyr::summarize(stat = sum((table(!!(attr(x, "response")))
                                     - expected)^2 / expected, ...))
    } else {
      # Straight from `specify()`
      x %>% 
        dplyr::summarize(stat = stats::chisq.test(
          table(!!(attr(x, "response"))))$stat
        )
    }

  } else {
    # This is not matching with chisq.test
    # obs_tab <- x %>%
    #   dplyr::filter(replicate == 1) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::select(!!attr(x, "response"),
    #                 !!(attr(x, "explanatory"))) %>%
    #   table()
    # expected <- outer(rowSums(obs_tab), colSums(obs_tab)) / n
    # df_out <- x %>%
    #   dplyr::summarize(stat = sum((table(!!(attr(x, "response")),
    #                                      !!(attr(x, "explanatory")))
    #                                - expected)^2 / expected, ...))
    
    
    result <- x %>%
      dplyr::do(broom::tidy(suppressWarnings(stats::chisq.test(
        table(.[[as.character(attr(x, "response"))]],
              .[[as.character(attr(x, "explanatory"))]]))))) %>%
      dplyr::ungroup()
    
    if(!is.null(attr(x, "params")))
      result <- result %>% dplyr::select(replicate, stat = statistic)
    else
      result <- result %>% dplyr::select(stat = statistic)
    
    attr(result, "response") <- attr(x, "response")
    attr(result, "success") <- attr(x, "success")
    attr(result, "explanatory") <- attr(x, "explanatory")
    attr(result, "response_type") <- attr(x, "response_type")
    attr(result, "explanatory_type") <- attr(x, "explanatory_type")
    attr(result, "distr_param") <- attr(x, "distr_param")
    attr(result, "distr_param2") <- attr(x, "distr_param2")
    attr(result, "theory_type") <- attr(x, "theory_type")
    
    result
    
  }
}

calc_impl.diff_in_props <- function(stat, x, order, ...) {

  col <- attr(x, "response")
  success <- attr(x, "success")
  
  x %>%
    dplyr::group_by(replicate, !!(attr(x, "explanatory"))) %>%
    dplyr::summarize(prop = mean(!!sym(col) == success, ...)) %>%
    dplyr::summarize(stat = prop[!!(attr(x, "explanatory")) == order[1]]
                     - prop[!!(attr(x, "explanatory")) == order[2]])
}

calc_impl.t <- function(stat, x, order, ...) {
  # Two sample means
  if (attr(x, "theory_type") == "Two sample t"){
    df_out <- x %>%
      dplyr::summarize(stat = stats::t.test(
        !! attr(x, "response") ~ !! attr(x, "explanatory"))[["statistic"]])
  }
    
  # Standardized slope
  else if (attr(x, "theory_type") == "Slope with t"){
    explan_string <- as.character(attr(x, "explanatory"))
  
    x %>%
      dplyr::summarize(stat = summary(
          stats::lm(!!(attr(x, "response")) ~ !!(attr(x, "explanatory")))
        )[["coefficients"]][explan_string, "t value"])
  } 
  # One sample mean (Not currently implemented)
  # else if (attr(x, "theory_type") == "One sample t"){
  #   df_out <- x %>%
  #     dplyr::summarize(stat = stats::t.test(
  #       response_variable(x))[["statistic"]])
  # }
}

calc_impl.z <- function(stat, x, order, ...) {
  # Two sample proportions
  if (attr(x, "theory_type") == "Two sample props z"){
    
    col <- attr(x, "response")
    success <- attr(x, "success")
    
    x$explan <- factor(explanatory_variable(x), 
                       levels = c(order[1], order[2]))
    
    aggregated <- x %>%
      dplyr::group_by(replicate, explan) %>%
      dplyr::summarize(group_num = n(),
                       prop = mean(rlang::eval_tidy(col) ==
                                     rlang::eval_tidy(success)),
                       num_suc = sum(rlang::eval_tidy(col) ==
                                       rlang::eval_tidy(success))
      ) 
    
    df_out <- aggregated %>%
      dplyr::summarize(diff_prop = prop[explan == order[1]] 
                       - prop[explan == order[2]],
                       total_suc = sum(num_suc),
                       n1 = group_num[1],
                       n2 = group_num[2],
                       p_hat = total_suc / (n1 + n2),
                       denom = sqrt(p_hat * (1 - p_hat) / n1
                                    + p_hat * (1 - p_hat) / n2),
                       stat = diff_prop / denom) %>%
      dplyr::select(-total_suc, -n1, -n2)
    
  } else
    # One sample proportion
    if (attr(x, "theory_type") == "One sample prop z"){
 #     if(!is.null(attr(x, "params"))){
        # When `hypothesize()` has been called
        
        success <- attr(x, "success")
        
        p0 <- attr(x, "params")[1]
        num_rows <- nrow(x) / length(unique(x$replicate))
        
        col <- attr(x, "response")
        #    if(is.null(success))
        #      success <- quo(get_par_levels(x)[1])
        # Error given instead
        
        df_out <- x %>%
          dplyr::summarize(stat = (mean(
            rlang::eval_tidy(col) == rlang::eval_tidy(success), ...) - p0
          ) / sqrt( (p0 * (1 - p0)) / num_rows))
#      } else
#        # Straight from `specify()`
        
    }
}

explanatory_variable <- function(x) {
  x[[as.character(attr(x, "explanatory"))]]
}

# `explanatory_variable<-` <- function(x, value) {
#   x[[as.character(attr(x, "explanatory"))]] <- value
# }

response_variable <- function(x) {
  x[[as.character(attr(x, "response"))]]
}

# `response_variable<-` <- function(x, value) {
#   x[[as.character(attr(x, "response"))]] <- value
# }

has_explanatory <- function(x){
  !is.null(attr(x, "explanatory"))
}

has_response <- function(x){
  !is.null(attr(x, "response"))
}

check_order <- function(x, explanatory_variable, order){
  unique_explanatory_variable <- unique(explanatory_variable)
  if (length(unique_explanatory_variable) != 2){
    stop(paste("Statistic is based on a difference; the explanatory variable",
               "should have two levels."))
  }
  if(is.null(order)){
    stop(paste("Statistic is based on a difference; specify the `order` in",
               "which to subtract the levels of the explanatory variable.",
               '`order = c("first", "second")` means `("first" - "second")`',
               "Check `?calculate` for details."))
  } else {
    if(xor(is.na(order[1]), is.na(order[2])))
      stop(paste("Only one level specified in `order`.",
                 "Both levels need to be specified."))
    if(length(order) > 2)
      stop("`order` is expecting only two entries.")
    if(order[1] %in% unique_explanatory_variable == FALSE)
      stop(paste(order[1], "is not a level of the explanatory variable."))
    if(order[2] %in% unique_explanatory_variable == FALSE)
      stop(paste(order[2], "is not a level of the explanatory variable."))
  }
}

check_args_and_attr <- function(x, explanatory_variable, response_variable, 
                                stat){
  
  # Could also do `stat <- match.arg(stat)`
  # but that's not as helpful to beginners with the cryptic error msg
  if (!stat %in% c("mean", "median", "sd", "prop",
                   "diff in means", "diff in medians", "diff in props",
                   "Chisq", "F", "slope", "t", "z")){
    stop(paste("You specified a string for `stat` that is not implemented.",
               "Check your spelling and `?calculate` for current options."))
  }
  
  if (!("replicate" %in% names(x)) && !is.null(attr(x, "generate")))
    warning(paste0('A `generate()` step was not performed prior to',
                   '`calculate()`. Review carefully.'))
  
  if (stat %in% c("F", "slope", "diff in means", "diff in medians")){
    if (has_explanatory(x) && !is.numeric(response_variable(x))){
      stop(paste0("The response variable of `",
                  attr(x, "response"),
                  "` is not appropriate \n  since '",
                  stat,
                  "' is expecting the response variable to be numeric."))
    }
  }
  
  if (stat %in% c("diff in props", "Chisq")){
    if (has_explanatory(x) && !is.factor(response_variable(x))){
      stop(paste0("The response variable of `",
                  attr(x, "response"),
                  "` is not appropriate \n  since '",
                  stat,
                  "' is expecting the response variable to be a factor."))
    }
  }
}

check_for_numeric_stat <- function(x, stat){
  if (stat %in% c("mean", "median", "sd")){
    col <- setdiff(names(x), "replicate")
    
    if (!is.numeric(x[[as.character(col)]])){
      stop(paste0("Calculating a ",
                  stat,
                  " here is not appropriate \n since the `",
                  col,
                  "` variable is not numeric."))
    }
  }
}

check_for_factor_stat <- function(x, stat, explanatory_variable){

  if (stat %in% c("diff in means", "diff in medians", "diff in props", "F")){
    if (!is.factor(explanatory_variable)){
      stop(paste0("The explanatory variable of `",
                  attr(x, "explanatory"),
                  "` is not appropriate \n since '",
                  stat,
                  "' is expecting the explanatory variable to be a factor."))
    }
  }
}
