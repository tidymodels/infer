# Miscellaneous Helpers -----------------------------------------------
append_infer_class <- function(x) {
  x_cl <- class(x)
  if (x_cl[1] != "infer") {
    class(x) <- c("infer", x_cl)
  }

  x
}

format_params <- function(x) {
  par_levels <- get_par_levels(x)
  fct_levels <- as.character(unique(response_variable(x)))
  attr(x, "params")[match(fct_levels, par_levels)]
}

print_params <- function(x) {
  params <- attr(x, "params")
  
  switch(
    as.character(length(params)),
    "1" = glue_null(": `{names(params)} = {unname(params)}`"),
    "2" = glue_null(": `p = .5`"),
          glue_null("s: `p = c({put_params(x, params)})`")
  )
}

put_params <- function(x, params) {
  paste0(get_par_levels(x), " = ", params, collapse = ", ")
}

get_par_levels <- function(x) {
  par_names <- names(attr(x, "params"))
  gsub("^.\\.", "", par_names)
}

copy_attrs <- function(to, from,
                       attrs = c(
                         "response", "success", "explanatory", "response_type",
                         "explanatory_type", "distr_param", "distr_param2",
                         "null", "params", "theory_type", "generated", "type",
                         "hypothesized"
                       )) {
  for (at in attrs) {
    attr(to, at) <- attr(from, at)
  }

  to
}

# Wrapper for deduplication by name after doing `c(...)`
c_dedupl <- function(...) {
  l <- c(...)
  
  l_names <- names(l)
  
  if (is.null(l_names)) {
    l
  } else {
    l[!duplicated(l_names) | (l_names == "")]
  }
}

reorder_explanatory <- function(x, order) {
  x[[explanatory_name(x)]] <- factor(
    explanatory_variable(x),
    levels = c(order[1], order[2])
  )
  x
}

# Getters, setters, and indicators ------------------------------------------
explanatory_expr <- function(x) {
  attr(x, "explanatory")
}

explanatory_name <- function(x) {
  as.character(explanatory_expr(x))
}

explanatory_variable <- function(x) {
  if (!is.null(explanatory_expr(x))) {
    x[[explanatory_name(x)]]
  } else {
    NULL
  }
}

response_expr <- function(x) {
  attr(x, "response")
}

response_name <- function(x) {
  as.character(response_expr(x))
}

response_variable <- function(x) {
  x[[response_name(x)]]
}

theory_type <- function(x) {
  attr(x, "theory_type")
}

get_response_levels <- function(x) {
  as.character(unique(response_variable(x)))
}

get_success_then_response_levels <- function(x) {
  success_attr <- attr(x, "success")
  response_levels <- setdiff(
    get_response_levels(x),
    success_attr
  )
  c(success_attr, response_levels)
}

has_unused_levels <- function(x) {
  present_levels <- unique(as.character(x))
  unused_levels <- setdiff(levels(x), present_levels)
  
  length(unused_levels) > 0
}

is_generated <- function(x) {
  attr(x, "generated")
}

is_hypothesized <- function(x){
  attr(x, "hypothesized")
}

attr_is_null <- function(x, at) {
  is.null(attr(x, at))
}

has_explanatory <- function(x) {
  !attr_is_null(x, "explanatory")
}

has_response <- function(x) {
  !attr_is_null(x, "response")
}

is_color_string <- function(x) {
  rlang::is_string(x) &&
    tryCatch(is.matrix(grDevices::col2rgb(x)), error = function(e) {FALSE})
}

# Messaging, warning, and erroring ------------------------------------------

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

# Helpers for test statistics --------------------------------------

# Simplify and standardize checks by grouping statistics based on variable types
# num = numeric, bin = binary (dichotomous), mult = multinomial
stat_types <- tibble::tribble(
  ~resp,   ~exp,   ~stats,
  "num",   "",     c("mean", "median", "sum", "sd", "t"),
  "num",   "num",  c("slope", "correlation"),
  "num",   "bin",  c("diff in means", "diff in medians", "t"),
  "num",   "mult", c("F"),
  "bin",   "",     c("prop", "count", "z"),
  "bin",   "bin",  c("diff in props", "z", "ratio of props", "odds ratio", "Chisq"),
  "bin",   "mult", c("Chisq"),
  "mult",  "bin",  c("Chisq"),
  "mult",  "",     c("Chisq"),
  "mult",  "mult", c("Chisq"),
)

stat_type_desc <- tibble::tribble(
  ~type,  ~description,
  "num",  "numeric",
  "bin",  "dichotomous categorical",
  "mult", "multinomial categorical"
)

stat_desc <- tibble::tribble(
  ~stat,               ~description,
  "mean",              "A mean",
  "median",            "A median", 
  "sum",               "A sum",
  "sd",                "A standard deviation",
  "prop",              "A proportion", 
  "count",             "A count", 
  "diff in means",     "A difference in means", 
  "diff in medians",   "A difference in medians", 
  "diff in props",     "A difference in proportions", 
  "Chisq",             "A chi-square statistic", 
  "F",                 "An F statistic", 
  "slope",             "A slope", 
  "correlation",       "A correlation", 
  "t",                 "A t statistic", 
  "z",                 "A z statistic", 
  "ratio of props",    "A ratio of proportions",  
  "odds ratio",        "An odds ratio" 
)

implemented_stats <-  c(
  "mean", "median", "sum", "sd", "prop", "count",
  "diff in means", "diff in medians", "diff in props",
  "Chisq", "F", "slope", "correlation", "t", "z",
  "ratio of props", "odds ratio"
)

untheorized_stats <- implemented_stats[!implemented_stats %in% c(
  "Chisq", "F", "t", "z"
)]

# Given a statistic and theory type, assume a reasonable null
p_null <- function(x) {
  lvls <- levels(response_variable(x))
  num_lvls <- length(lvls)
  probs <- 1 / num_lvls
  
  setNames(rep(probs, num_lvls), paste0("p.", lvls))
}

# The "null_fn" column is a function(x) whose output gives attr(x, "params")
theorized_nulls <- tibble::tribble(
  ~stat,    ~null_fn,
  "Chisq", p_null,
  "t",     function(x) {setNames(0, "mu")},
  "z",     p_null
)

determine_variable_type <- function(x, variable) {
  var <- switch(
    variable,
    response = response_variable(x),
    explanatory = explanatory_variable(x)
  )
  
  res <- if (is.null(var)) {
    ""
  } else if (inherits(var, "numeric")) {
    "num"
  } else if (length(unique(var)) == 2) {
    "bin"
  } else {
    "mult"
  }
  
  res
}

# Argument checking --------------------------------------------------------

check_order <- function(x, order, in_calculate = TRUE, stat) {
  # If there doesn't need to be an order argument, warn if there is one,
  # and otherwise, skip checks
  if (!(theory_type(x) %in% c("Two sample props z", "Two sample t") ||
        is.null(stat) ||
        stat %in% c("diff in means", "diff in medians", 
                    "diff in props", "ratio of props", "odds ratio"))) {
    if (!is.null(order)) {
       warning_glue(
        "Statistic is not based on a difference or ratio; the `order` argument",
        " will be ignored. Check `?calculate` for details."
      )
    } else {
      return(order)
    }
  } 
  
  explanatory_variable <- explanatory_variable(x)
  unique_ex <- sort(unique(explanatory_variable))
  
  if (is.null(order) & in_calculate) {
    # Default to subtracting/dividing the first (alphabetically) level by the 
    # second, unless the explanatory variable is a factor (in which case order 
    # is preserved); raise a warning if this was done implicitly.
    order <- as.character(unique_ex)
    warning_glue(
      "The statistic is based on a difference or ratio; by default, for ",
      "difference-based statistics, the explanatory variable is subtracted ",
      "in the order \"{unique_ex[1]}\" - \"{unique_ex[2]}\", or divided in ", 
      "the order \"{unique_ex[1]}\" / \"{unique_ex[2]}\" for ratio-based ",
      "statistics. To specify this order yourself, supply `order = ",
      "c(\"{unique_ex[1]}\", \"{unique_ex[2]}\")` to the calculate() function."
    )
  } else if (is.null(order)) {
    order <- as.character(unique_ex)
    warning_glue(
      "The statistic is based on a difference or ratio; by default, for ",
      "difference-based statistics, the explanatory variable is subtracted ",
      "in the order \"{unique_ex[1]}\" - \"{unique_ex[2]}\", or divided in ", 
      "the order \"{unique_ex[1]}\" / \"{unique_ex[2]}\" for ratio-based ",
      "statistics. To specify this order yourself, supply `order = ",
      "c(\"{unique_ex[1]}\", \"{unique_ex[2]}\")`."
    )    
  } else {
    if (xor(is.na(order[1]), is.na(order[2]))) {
      stop_glue(
        "Only one level specified in `order`. Both levels need to be specified."
      )
    }
    if (length(order) > 2) {
      stop_glue("`order` is expecting only two entries.")
    }
    if (order[1] %in% unique_ex == FALSE) {
      stop_glue("{order[1]} is not a level of the explanatory variable.")
    }
    if (order[2] %in% unique_ex == FALSE) {
      stop_glue("{order[2]} is not a level of the explanatory variable.")
    }
  }
  # return the order as given (unless the argument was invalid or NULL)
  order
}

check_point_params <- function(x, stat) {
  param_names <- attr(attr(x, "params"), "names")
  hyp_text <- 'to be set in `hypothesize()`.'
  if (is_hypothesized(x)) {
    if (stat %in% c("mean", "median", "sd", "prop")) {
      if ((stat == "mean") && !("mu" %in% param_names)) {
        stop_glue('`stat == "mean"` requires `"mu"` {hyp_text}')
      }
      if (!(stat == "mean") && ("mu" %in% param_names)) {
        stop_glue('`"mu"` does not correspond to `stat = "{stat}"`.')
      }
      if ((stat == "median") && !("med" %in% param_names)) {
        stop_glue('`stat == "median"` requires `"med"` {hyp_text}')
      }
      if (!(stat == "median") && ("med" %in% param_names)) {
        stop_glue('`"med"` does not correspond to `stat = "{stat}"`.')
      }
    }
  }
}

# This function checks for NaNs in the output of `calculate` and raises
# a message/warning/error depending on the context in which it was called.
check_for_nan <- function(x, context) {
  stat_is_nan <- is.nan(x[["stat"]])
  num_nans <- sum(stat_is_nan)
  # If there are no NaNs, continue on as normal :-)
  if (num_nans == 0) {
    return(x)
  }
  
  calc_ref <- "See ?calculate for more details"
  # If all of the data is NaN, raise an error
  if (num_nans == nrow(x)) {
    stop_glue("All calculated statistics were `NaN`. {calc_ref}.")
  }
  
  stats_were <- if (num_nans == 1) {"statistic was"} else {"statistics were"}
  num_nans_msg <- glue::glue("{num_nans} calculated {stats_were} `NaN`")
  
  if (context == "visualize") {
    # Raise a warning and plot the data with NaNs removed
    warning_glue(
      "{num_nans_msg}. `NaN`s have been omitted from visualization. {calc_ref}."
    )
    return(x[!stat_is_nan, ])
  } else if (context == "get_p_value") {
    # Raise an error
    stop_glue(
      "{num_nans_msg}. Simulation-based p-values are not well-defined for ",
      "null distributions with non-finite values. {calc_ref}."
    )
  }
}

check_direction <- function(direction = c("less", "greater", "two_sided",
                                          "left", "right", "both",
                                          "two-sided", "two sided", 
                                          "two.sided")) {
  check_type(direction, is.character)

  if (
    !(direction %in% c("less", "greater", "two_sided", "left", "right", 
                       "both", "two-sided", "two sided", "two.sided"))
  ) {
    stop_glue(
      'The provided value for `direction` is not appropriate. Possible values ',
      'are "less", "greater", "two-sided", "left", "right", "both", ',
      '"two_sided", "two sided", or "two.sided".'
    )
  }
}

check_obs_stat <- function(obs_stat) {
  if (!is.null(obs_stat)) {
    
    if ("data.frame" %in% class(obs_stat)) {
      check_type(obs_stat, is.data.frame)
      if ((nrow(obs_stat) != 1) || (ncol(obs_stat) != 1)) {
        warning_glue(
          "The first row and first column value of the given `obs_stat` will ",
          "be used."
        )
      }

      # [[1]] is used in case `stat` is not specified as name of 1x1
      obs_stat <- obs_stat[[1]][[1]]
      check_type(obs_stat, is.numeric)
    } else {
      check_type(obs_stat, is.numeric)
    }
  }

  obs_stat
}

#' Check object type
#'
#' Throw an error in case object is not of desired type.
#'
#' @param x An object to check.
#' @param predicate A function to perform check. A good idea is to use function
#'   named `is.*()` or `is_*()` with possible `<package>::` prefix.
#' @param type A string for desired type. If `NULL`, type is taken from parsing
#'   original name of supplied `predicate`: all alphanumeric with '_' and '.'
#'   characters (until the name end) after the first appearance of either `is.`
#'   or `is_`. In case of a doubt supply `type` explicitly.
#'
#' @examples
#' \dontrun{
#' x <- 1
#' check_type(x, is.numeric)
#' check_type(x, is.logical)
#' check_type(x, rlang::is_string, "character of length 1")
#' }
#'
#' @keywords internal
#' @noRd
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
