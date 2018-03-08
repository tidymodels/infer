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
                   "Chisq", "F", "slope", "t", "z")){
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
  
  if (stat %in% c("diff in means", "diff in medians", "diff in props") |
      attr(x, "theory_type") %in% c("Two sample props z", "Two sample t")) {
    if (length(unique(x[[as.character(attr(x, "explanatory"))]])) != 2){
      stop(paste("Statistic is based on a difference; the explanatory variable",
                 "should have two levels."))
    }
    if (is.null(order)){
      stop(paste("Statistic is based on a difference; specify the `order` in",
                 "which to subtract the levels of the explanatory variable.",
                 "Check `?calculate` for details."))
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
  
  # add "infer" class
  class(x) <- append("infer", class(x))
  
  # Use S3 method to match correct calculation
  calc_impl(
    structure(stat, class = gsub(" ", "_", stat)), x, order, ...
  )
  
  
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
  col <- attr(x, "response")
  
  if(!is.factor(x[[as.character(col)]])){
    stop(paste0("Calculating a ",
                stat,
                " here is not appropriate \n  since the `",
                col,
                "` variable is not a factor."))
  }
  
  success <- attr(x, "success")
  x %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarize(stat = mean(rlang::eval_tidy(col) ==
                                   rlang::eval_tidy(success),
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
  n   <- attr(x, "biggest_group_size")
  
  if (is.null(attr(x, "explanatory"))) {
    expected <- n * attr(x, "params")
    x %>%
      dplyr::summarize(stat = sum((table(!!(attr(x, "response")))
                                   - expected)^2 / expected, ...))
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
    
    x %>%
      dplyr::group_by(replicate) %>%
      dplyr::do(broom::tidy(suppressWarnings(stats::chisq.test(
        table(.[[as.character(attr(x, "response"))]],
              .[[as.character(attr(x, "explanatory"))]]))))) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(replicate = as.factor(replicate), stat = statistic)
  }
}

calc_impl.diff_in_props <- function(stat, x, order, ...) {
  
  # Detected in another test
  #    if (length(levels(x[[as.character(attr(x, "explanatory"))]])) != 2){
  #      stop(paste0("The explanatory variable of `",
  #                  attr(x, "explanatory"),
  #                  "` does not have exactly two levels. \n",
  #                  "Convert it to have only two levels and try again."))
  #    }
  
  col <- attr(x, "response")
  success <- attr(x, "success")
  
  x %>%
    dplyr::group_by(replicate, !!(attr(x, "explanatory"))) %>%
    dplyr::summarize(prop = mean(rlang::eval_tidy(col) ==
                                   rlang::eval_tidy(success), ...)) %>%
    dplyr::summarize(stat = prop[!!(attr(x, "explanatory")) == order[1]]
                     - prop[!!(attr(x, "explanatory")) == order[2]])
}

calc_impl.t <- function(stat, x, order, ...) {
  # Two sample means
  if (attr(x, "theory_type") == "Two sample t"){
    df_out <- x %>%
      dplyr::summarize(stat = stats::t.test(
        !! attr(x, "response") ~ !! attr(x, "explanatory"))[["statistic"]])
    
    # One sample mean (TESTING - not currently working)
  } else if (attr(x, "theory_type") == "One sample t"){
    df_out <- x %>%
      dplyr::summarize(stat = stats::t.test(
        x[[as.character(attr(x, "response"))]])[["statistic"]])
  }
}

calc_impl.z <- function(stat, x, order, ...) {
  # Two sample proportions
  if (attr(x, "theory_type") == "Two sample props z"){
    
    col <- attr(x, "response")
    success <- attr(x, "success")
    
    x$explan <- factor(x[[attr(x, "explanatory")]], 
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
    
    # df_out <- x %>%
    #   dplyr::group_by(replicate, !!attr(x, "explanatory")) %>%
    #   dplyr::summarize(prop = mean((!!attr(x, "response")) == levels(!!attr(x, "response"))[1], ...),
    #                    num_suc = sum((!!attr(x, "response")) == levels(!!attr(x, "response"))[1], ...),
    #                    group_num = n()) %>%
    #   dplyr::summarize(diff_prop = diff(prop),
    #                    total_suc = sum(num_suc),
    #                    n1 = group_num[1],
    #                    n2 = group_num[2],
    #                    p_hat = total_suc / (n1 + n2),
    #                    denom = sqrt(p_hat * (1 - p_hat) / n1 + p_hat * (1 - p_hat) / n2),
    #                    stat = diff_prop / denom) %>%
    #   dplyr::select(-total_suc, -n1, -n2)
    
  } else
    # One sample proportion
    if (attr(x, "theory_type") == "One sample prop z"){
      
      success <- attr(x, "success")
      
      p0 <- attr(x, "params")[1]
      num_rows <- nrow(x) / length(unique(x$replicate))
      
      col <- attr(x, "response")
      #    if(is.null(success))
      #      success <- quo(get_par_levels(x)[1])
      # Error given instead
      
      df_out <- x %>%
        dplyr::summarize(stat =
                           (mean(rlang::eval_tidy(col) == rlang::eval_tidy(success), ...) - p0) /
                           sqrt( (p0 * (1 - p0)) / num_rows))
    }
}