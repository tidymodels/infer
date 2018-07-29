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

stop_glue <- function(..., .sep = "", .envir = parent.frame(),
                      call. = FALSE, .domain = NULL) {
  stop(
    glue_null(..., .sep = .sep, .envir = .envir),
    call. = call., domain = .domain
  )
}

warning_glue <- function(..., .sep = "", .envir = parent.frame(),
                         call. = FALSE, .domain = NULL) {
  warning(
    glue_null(..., .sep = .sep, .envir = .envir),
    call. = call., domain = .domain
  )
}

message_glue <- function(..., .sep = "", .envir = parent.frame(),
                         .domain = NULL, .appendLF = TRUE) {
  message(
    glue_null(..., .sep = .sep, .envir = .envir),
    domain = .domain, appendLF = .appendLF
  )
}

glue_null <- function(..., .sep = "", .envir = parent.frame()) {
  glue::glue(
    ..., .sep = .sep, .envir = .envir, .transformer = null_transformer
  )
}

# This allows to print 'NULL' in `glue()` for code which evaluates in `NULL`
null_transformer <- function(text, envir) {
  out <- eval(parse(text = text, keep.source = FALSE), envir)
  if (is.null(out)) {
    return("NULL")
  }
  
  out
}

check_order <- function(x, explanatory_variable, order){
  unique_explanatory_variable <- unique(explanatory_variable)
  if (length(unique_explanatory_variable) != 2){
    stop_glue("Statistic is based on a difference; the explanatory variable ",
              "should have two levels.")
  }
  if(is.null(order)){
    stop_glue("Statistic is based on a difference; specify the `order` in ",
              "which to subtract the levels of the explanatory variable. ",
              '`order = c("first", "second")` means `("first" - "second")`. ',
              "Check `?calculate` for details.")
  } else {
    if(xor(is.na(order[1]), is.na(order[2])))
      stop_glue(
        "Only one level specified in `order`. Both levels need to be specified."
      )
    if(length(order) > 2)
      stop_glue("`order` is expecting only two entries.")
    if(order[1] %in% unique_explanatory_variable == FALSE)
      stop_glue("{order[1]} is not a level of the explanatory variable.")
    if(order[2] %in% unique_explanatory_variable == FALSE)
      stop_glue("{order[2]} is not a level of the explanatory variable.")
  }
}

check_args_and_attr <- function(x, explanatory_variable, response_variable, 
                                stat){
  
  # Could also do `stat <- match.arg(stat)`
  # but that's not as helpful to beginners with the cryptic error msg
  if (!stat %in% c("mean", "median", "sd", "prop",
                   "diff in means", "diff in medians", "diff in props",
                   "Chisq", "F", "slope", "correlation", "t", "z")){
    stop_glue("You specified a string for `stat` that is not implemented. ",
              "Check your spelling and `?calculate` for current options.")
  }
  
  if (!("replicate" %in% names(x)) && !is.null(attr(x, "generate")))
    warning_glue('A `generate()` step was not performed prior to ',
                 '`calculate()`. Review carefully.')
  
  if (stat %in% c("F", "slope", "diff in means", "diff in medians")){
    if (has_explanatory(x) && !is.numeric(response_variable(x))){
      stop_glue(
        'The response variable of `{attr(x, "response")}` is not appropriate\n',
        "since '{stat}' is expecting the response variable to be numeric."
      )
    }
  }
  
  if (stat %in% c("diff in props", "Chisq")){
    if (has_explanatory(x) && !is.factor(response_variable(x))){
      stop_glue(
        'The response variable of `{attr(x, "response")}` is not appropriate\n',
        "since '{stat}' is expecting the response variable to be a factor."
      )
    }
  }
}

check_for_numeric_stat <- function(x, stat){
  if (stat %in% c("mean", "median", "sd")){
    col <- base::setdiff(names(x), "replicate")
    
    if (!is.numeric(x[[as.character(col)]])){
      stop_glue(
        "Calculating a {stat} here is not appropriate\n",
        "since the `{col}` variable is not numeric."
      )
    }
  }
}

check_for_factor_stat <- function(x, stat, explanatory_variable){
  
  if (stat %in% c("diff in means", "diff in medians", "diff in props", "F")){
    if (!is.factor(explanatory_variable)){
      stop_glue(
        'The explanatory variable of `{attr(x, "explanatory")}` is not ',
        "appropriate\n",
        "since '{stat}` is expecting the explanatory variable to be a factor."
      )
    }
  }
}

check_point_params <- function(x, stat){
  
  param_names <- attr(attr(x, "params"), "names") 
  hyp_text <- 'to be set in `hypothesize()`.'
  if(!is.null(attr(x, "null"))){
    if(stat %in% c("mean", "median", "sd", "prop")){
      if( (stat == "mean" && !("mu" %in% param_names)) )
        stop_glue('`stat == "mean"` requires `"mu"` {hyp_text}')
      if ( (!(stat == "mean") && ("mu" %in% param_names)) )
        stop_glue('`"mu"` does not correspond to `stat = "{stat}"`.')
      if( (stat == "median" && !("med" %in% param_names) ) )
        stop_glue('`stat == "median"` requires `"med"` {hyp_text}')
      if ( (!(stat == "median") && ("med" %in% param_names)) )
        stop_glue('`"med"` does not correspond to `stat = "{stat}"`.')
      ## Tests unable to get to
      # if( (stat == "sigma" && !("sd" %in% param_names)) )
      #   stop_glue('`stat == "sd"` requires `"sigma"` {hyp_text}')
      if ( (!(stat == "sd") && ("sigma" %in% param_names)) )
        stop_glue('`"sigma"` does not correspond to `stat = "{stat}"`.')
      
      ## Tests unable to get to
      # if(stat == "prop" && !(any(grepl("p.", param_names))))
      #   stop_glue('`stat == "prop"` requires `"p"` {hyp_text}')
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
    stop_glue(
      'Parameter values can be only one of `p`, `mu`, `med`, or `sigma`.'
    )
  }
  
  # add in 1 - p if it's missing
  # Outside if() is needed to ensure an error does not occur in referencing the
  # 0 index of dots
  if (length(p_ind)) {
    if (length(dots[[p_ind]]) == 1) {
      
      if (attr(x, "null") == "point" && is.null(attr(x, "success"))) {
        stop_glue("A point null regarding a proportion requires ",
                  "that `success` be indicated in `specify()`.")
      }
      if(dots$p < 0 || dots$p > 1)
        stop_glue(
          "The value suggested for `p` is not between 0 and 1, inclusive."
        )
      missing_lev <- base::setdiff(unique(pull(x, !!attr(x, "response"))), 
                             attr(x, "success"))
      dots$p <- append(dots$p, 1 - dots$p)
      names(dots$p) <- c(attr(x, "success"), missing_lev)
    } else {
      if(sum(dots$p) != 1){
        stop_glue("Make sure the hypothesized values for the `p` parameters ",
                  "sum to 1. Please try again.")
      }
    }
  }
  
  # if (sum(dots[[p_ind]]) != 1){
  #   dots[[p_ind]] <- dots[[p_ind]]/sum(dots[[p_ind]])
  #   warning_glue("Proportions do not sum to 1, normalizing automatically.")
  # }
  
  return(unlist(dots))
}

hypothesize_checks <- function(x, null){
  # error: x is not a dataframe
  if (!sum(class(x) %in% c("data.frame", "tbl", "tbl_df", "grouped_df"))) {
    stop_glue("x must be a data.frame or tibble")
  }
  
  # error: null not found
  if (!(null %in% c("independence", "point"))) {
    stop_glue(
      "Choice of null is not supported. Check `?hypothesize` for options."
    )
  }
  
  #  if (length(null) != 1) {
  #    stop_glue('Choose between either `"independence"` or `"point"` ',
  #              'for `null` argument.')
  #  }
  
  if(!has_response(x)){
    stop_glue(
      "The response variable is not set. Make sure to `specify()` it first."
    )
  }
  
  if(null == "independence" && !has_explanatory(x)){
    stop_glue('Please `specify()` an explanatory and a response variable ',
              'when testing\n',
              'a null hypothesis of `"independence"`.')
  }
}

check_direction <- function(direction = c("less", "greater", "two_sided",
                                          "left", "right", "both")){
  check_type(direction, is.character)
  
  if(!(direction %in% c("less", "greater", "two_sided",
                        "left", "right", "both"))){
    stop_glue('The provided value for `direction` is not appropriate. ',
              'Possible values are "less", "greater", "two_sided", ',
              '"left", "right", or "both".')
  }
}

check_obs_stat <- function(obs_stat){
  if(!is.null(obs_stat)){
    if("data.frame" %in% class(obs_stat)){
      check_type(obs_stat, is.data.frame)
      if( (nrow(obs_stat) != 1) || (ncol(obs_stat) != 1) ) 
        warning_glue("The first row and first column value of the given ", 
                     "`obs_stat` will be used.")
      
      # [[1]] is used in case `stat` is not specified as name of 1x1
      obs_stat <- obs_stat[[1]][[1]]
      check_type(obs_stat, is.numeric)
    }
    else{
      check_type(obs_stat, is.numeric)
    }
  }
  
  obs_stat
}

#' Check object type
#' 
#' Throw an error in case object is not of desired type.
#' 
#' @param x an object to check
#' @param predicate a function to perform check. A good idea is to use function
#'  named `is.*()` or `is_*()` with possible `<package>::`
#'  prefix.
#' @param type a string for desired type. If `NULL`, type is taken from
#'  parsing original name of supplied `predicate`: all alphanumeric with
#'  '_' and '.' characters (until the name end) after the first appearance of
#'  either `is.` or `is_`. In case of a doubt supply `type`
#'  explicitly.
#' @examples
#' #' \dontrun{
#' x <- 1
#' check_type(x, is.numeric)
#' check_type(x, is.logical)
#' check_type(x, rlang::is_string, "character of length 1")
#' }
#' @noRd
#' @keywords internal
check_type <- function(x, predicate, type = NULL) {
  x_name <- deparse(rlang::enexpr(x))
  if (is.null(type)) {
    predicate_name <- deparse(rlang::enexpr(predicate))
    type <- parse_type(predicate_name)
  }
  
  if (!isTRUE(predicate(x))) {
    # Not using "must be of type" because of 'tibble' and 'string' cases
    stop_glue("`{x_name}` must be '{type}', not '{get_type(x)}'.")
  }
  
  x
}

# This function is needed because `typeof()` on data frame returns "list"
get_type <- function(x) {
  if (is.data.frame(x)) {
    return("data.frame")
  }
  
  typeof(x)
}

parse_type <- function(f_name) {
  regmatches(
    f_name,
    regexec("is[_\\.]([[:alnum:]_\\.]+)$", f_name)
  )[[1]][2]
}
