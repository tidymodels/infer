format_params <- function(x) {
  par_levels <- get_par_levels(x)
  fct_levels <- as.character(unique(dplyr::pull(x, !! attr(x, "response"))))
  return(attr(x, "params")[match(fct_levels, par_levels)])
}

get_par_levels <- function(x) {
  par_names <- names(attr(x, "params"))
  return(gsub("^.\\.", "", par_names))
}

set_attributes <- function(to, from = x){
  attr(to, "response") <- attr(from, "response")
  attr(to, "success") <- attr(from, "success")
  attr(to, "explanatory") <- attr(from, "explanatory")
  attr(to, "response_type") <- attr(from, "response_type")
  attr(to, "explanatory_type") <- attr(from, "explanatory_type")
  attr(to, "distr_param") <- attr(from, "distr_param")
  attr(to, "distr_param2") <- attr(from, "distr_param2")
  attr(to, "null") <- attr(from, "null")
  attr(to, "params") <- attr(from, "params")
  attr(to, "theory_type") <- attr(from, "theory_type")
  attr(to, "generate") <- attr(from, "generate")
  attr(to, "type") <- attr(from, "type")
  
  return(to)
}

explanatory_variable <- function(x) {
  x[[as.character(attr(x, "explanatory"))]]
}

response_variable <- function(x) {
  x[[as.character(attr(x, "response"))]]
}

reorder_explanatory <- function(x, order){
  x[[as.character(attr(x, "explanatory"))]] <-
    factor(x[[as.character(attr(x, "explanatory"))]],
           levels = c(order[1], order[2]))
  x
}

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
               "Check `?calculate` for details."),
         call. = FALSE)
  } else {
    if(xor(is.na(order[1]), is.na(order[2])))
      stop(paste("Only one level specified in `order`.",
                 "Both levels need to be specified."),
           call. = FALSE)
    if(length(order) > 2)
      stop("`order` is expecting only two entries.",
           call. = FALSE)
    if(order[1] %in% unique_explanatory_variable == FALSE)
      stop(paste(order[1], "is not a level of the explanatory variable."),
           call. = FALSE)
    if(order[2] %in% unique_explanatory_variable == FALSE)
      stop(paste(order[2], "is not a level of the explanatory variable."),
           call. = FALSE)
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
               "Check your spelling and `?calculate` for current options."),
         call. = FALSE)
  }
  
  if (!("replicate" %in% names(x)) && !is.null(attr(x, "generate")))
    warning(paste0('A `generate()` step was not performed prior to',
                   '`calculate()`. Review carefully.'),
            call. = FALSE)
  
  if (stat %in% c("F", "slope", "diff in means", "diff in medians")){
    if (has_explanatory(x) && !is.numeric(response_variable(x))){
      stop(paste0("The response variable of `",
                  attr(x, "response"),
                  "` is not appropriate \n  since '",
                  stat,
                  "' is expecting the response variable to be numeric."),
           call. = FALSE)
    }
  }
  
  if (stat %in% c("diff in props", "Chisq")){
    if (has_explanatory(x) && !is.factor(response_variable(x))){
      stop(paste0("The response variable of `",
                  attr(x, "response"),
                  "` is not appropriate \n  since '",
                  stat,
                  "' is expecting the response variable to be a factor."),
           call. = FALSE)
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
                  "` variable is not numeric."),
           call. = FALSE)
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
                  "' is expecting the explanatory variable to be a factor."),
           call. = FALSE)
    }
  }
}

check_point_params <- function(x, stat){
  
  param_names <- attr(attr(x, "params"), "names") 
  hyp_text <- ' to be set in `hypothesize()`.'
  if(!is.null(attr(x, "null"))){
    if(stat %in% c("mean", "median", "sd", "prop")){
      if( (stat == "mean" && !("mu" %in% param_names)) )
        stop(paste0('`stat == "mean"` requires `"mu"`', hyp_text),
             call. = FALSE)
      if ( (!(stat == "mean") && ("mu" %in% param_names)) )
        stop(paste0('`"mu"` does not correspond to `stat = "', stat, '"`.'),
             call. = FALSE)
      if( (stat == "median" && !("med" %in% param_names) ) )
        stop(paste0('`stat == "median"` requires `"med"`', hyp_text),
             call. = FALSE)
      if ( (!(stat == "median") && ("med" %in% param_names)) )
        stop(paste0('`"med"` does not correspond to `stat = "', stat, '"`.'),
             call. = FALSE)
      ## Tests unable to get to
      # if( (stat == "sigma" && !("sd" %in% param_names)) )
      #   stop(paste0('`stat == "sd"` requires `"sigma"`', hyp_text),
      #        call. = FALSE)
      if ( (!(stat == "sd") && ("sigma" %in% param_names)) )
        stop(paste0('`"sigma"` does not correspond to `stat = "', stat, '"`.'),
             call. = FALSE)
      
      ## Tests unable to get to
      # if(stat == "prop" && !(any(grepl("p.", param_names))))
      #   stop(paste0('`stat == "prop"` requires `"p"`', hyp_text),
      #        call. = FALSE)
    }
  }
}

parse_params <- function(dots, x) {
  p_ind <- grep("p", names(dots))
  mu_ind <- grep("mu", names(dots))
  med_ind <- grep("med", names(dots))
  sig_ind <- grep("sigma", names(dots))
  
  # error: cannot specify more than one of props, means, medians, or sds
  if ( length(p_ind) + length(mu_ind) + length(med_ind) 
       + length(sig_ind) != 1 ){
    stop('Parameter values can be only one of `p`, `mu`, `med`, or `sigma`.',
         call. = FALSE)
  }
  
  # add in 1 - p if it's missing
  # Outside if() is needed to ensure an error does not occur in referencing the
  # 0 index of dots
  if (length(p_ind)) {
    if (length(dots[[p_ind]]) == 1) {
      
      if (attr(x, "null") == "point" && is.null(attr(x, "success"))) {
        stop(paste("A point null regarding a proportion requires",
                   "that `success` be indicated in `specify()`."),
             call. = FALSE)
      }
      if(dots$p < 0 || dots$p > 1)
        stop("The value suggested for `p` is not between 0 and 1, inclusive.",
             call. = FALSE)
      missing_lev <- setdiff(unique(pull(x, !!attr(x, "response"))), 
                             attr(x, "success"))
      dots$p <- append(dots$p, 1 - dots$p)
      names(dots$p) <- c(attr(x, "success"), missing_lev)
    } else {
      if(sum(dots$p) != 1){
        stop(paste("Make sure the hypothesized values for the `p` parameters",
                   "sum to 1. Please try again."),
             call. = FALSE)
      }
    }
  }
  
  # if (sum(dots[[p_ind]]) != 1){
  #   dots[[p_ind]] <- dots[[p_ind]]/sum(dots[[p_ind]])
  #   warning("Proportions do not sum to 1, normalizing automatically.")
  # }
  
  return(unlist(dots))
}

hypothesize_checks <- function(x, null){
  # error: x is not a dataframe
  if (!sum(class(x) %in% c("data.frame", "tbl", "tbl_df", "grouped_df"))) {
    stop("x must be a data.frame or tibble",
         call. = FALSE)
  }
  
  # error: null not found
  if (!(null %in% c("independence", "point"))) {
    stop("Choice of null is not supported. Check `?hypothesize` for options.",
         call. = FALSE)
  }
  
  #  if (length(null) != 1) {
  #    stop(paste0('Choose between either `"independence"` or `"point"`',
  #                 'for `null` argument.')
  #  }
  
  if(!has_response(x)){
    stop(paste("The response variable is not set.",
               "Make sure to `specify()` it first."),
         call. = FALSE)
  }
  
  if(null == "independence" && !has_explanatory(x)){
    stop(paste0('Please `specify()` an explanatory and a response variable ',
                'when testing \n',
                'a null hypothesis of `"independence"`.'),
         call. = FALSE)
  }
}
