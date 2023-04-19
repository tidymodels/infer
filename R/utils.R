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
    "1" = glue(": `{names(params)} = {unname(params)}`", .null = "NULL"),
    "2" = glue(": `p = .5`", .null = "NULL"),
          glue("s: `p = c({put_params(x, params)})`", .null = "NULL")
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
                         "hypothesized", "formula", "fitted",
                         "type_desc_response", "type_desc_explanatory"
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

standardize_variable_types <- function(x) {
   tibble::as_tibble(x) %>%
      # character and ordered to factor
      dplyr::mutate(
         dplyr::across(
            where(~ is.character(.x) || is.ordered(.x)),
            ~ factor(.x, ordered = FALSE)
         )
      ) %>%
      # logical to factor, with TRUE as the first level
      dplyr::mutate(
         dplyr::across(
            where(~ is.logical(.x)),
            ~ factor(.x, levels = c("TRUE", "FALSE"))
         )
      )  %>%
      # integer to numeric
      dplyr::mutate(
         dplyr::across(
            where(is.integer),
            as.numeric
         )
      )
}

# Performant grouping ----------------------------------------------------------
group_by_replicate <- function(tbl, reps, n) {
   dplyr::new_grouped_df(
      tbl,
      groups = make_replicate_groups(tbl, reps = reps, n = n)
   )
}


make_replicate_groups <- function(tbl, reps, n) {
   res <-
      tibble::new_tibble(list(
         replicate = 1:reps,
         .rows =
            vctrs::as_list_of(
               vctrs::vec_chop(seq_len(n*reps), sizes = rep(n, reps)),
               .ptype = integer()
            )
      ))

   attr(res, ".drop") <- TRUE

   res
}

# Getters, setters, and indicators ------------------------------------------
explanatory_expr <- function(x) {
  attr(x, "explanatory")
}

explanatory_name <- function(x) {
  all.vars(explanatory_expr(x))
}

# if there is more than one explanatory variable, return a data frame.
# if there's one, return a vector. otherwise, return NULL.
explanatory_variable <- function(x) {
  if (!is.null(explanatory_expr(x))) {
    if (length(explanatory_name(x)) > 1) {
      x[explanatory_name(x)]
    } else {
      x[[explanatory_name(x)]]
    }
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

is_generated <- function(x) {
  isTRUE(attr(x, "generated"))
}

is_hypothesized <- function(x){
  isTRUE(attr(x, "hypothesized"))
}

is_fitted <- function(x){
  isTRUE(attr(x, "fitted"))
}

is_mlr <- function(x) {
  length(explanatory_name(x)) > 1
}

has_attr <- function(x, at) {
  !is.null(attr(x, at, exact = TRUE))
}

has_explanatory <- function(x) {
  has_attr(x, "explanatory")
}

has_response <- function(x) {
  has_attr(x, "response")
}

is_color_string <- function(x) {
  rlang::is_string(x) &&
    tryCatch(is.matrix(grDevices::col2rgb(x)), error = function(e) {FALSE})
}

is_single_number <- function(x, min_val = -Inf, max_val = Inf,
                             include_min_val = TRUE, include_max_val = TRUE) {
  left_compare <- if (include_min_val) {`>=`} else {`>`}
  right_compare <- if (include_max_val) {`<=`} else {`<`}

  is.numeric(x) && (length(x) == 1) && is.finite(x) &&
    left_compare(x, min_val) && right_compare(x, max_val)
}

is_truefalse <- function(x) {
  identical(x, TRUE) || identical(x, FALSE)
}

# Helpers for test statistics --------------------------------------

# Simplify and standardize checks by grouping statistics based on variable types
# num = numeric, bin = binary (dichotomous), mult = multinomial
stat_types <- tibble::tribble(
  ~resp,   ~exp,   ~stats,
  "num",   "",     c("mean", "median", "sum", "sd", "t"),
  "num",   "num",  c("slope", "correlation"),
  "num",   "bin",  c("diff in means", "diff in medians", "t", "ratio of means"),
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

get_stat_type_desc <- function(stat_type) {
  stat_type_desc$description[stat_type_desc$type == stat_type]
}

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
  "ratio of means",    "A ratio of means",
  "odds ratio",        "An odds ratio"
)

stat_hypotheses <- tibble::tribble(
  ~stat,               ~hypothesis,
  "mean",              "point",
  "median",            "point",
  "sum",               "point",
  "sd",                "point",
  "prop",              "point",
  "count",             "point",
  "mean",              "paired independence",
  "median",            "paired independence",
  "sum",               "paired independence",
  "sd",                "paired independence",
  "diff in means",     "independence",
  "diff in medians",   "independence",
  "diff in props",     "independence",
  "Chisq",             "independence",
  "Chisq",             "point",
  "F",                 "independence",
  "slope",             "independence",
  "correlation",       "independence",
  "t",                 "independence",
  "t",                 "point",
  "z",                 "independence",
  "z",                 "point",
  "ratio of props",    "independence",
  "ratio of means",    "independence",
  "odds ratio",        "independence"
)

get_stat_desc <- function(stat) {
  stat_desc$description[stat_desc$stat == stat]
}

# Values of `stat` argument of `calculate()`
implemented_stats <-  c(
  "mean", "median", "sum", "sd", "prop", "count",
  "diff in means", "diff in medians", "diff in props",
  "Chisq", "F", "slope", "correlation", "t", "z",
  "ratio of props", "ratio of means", "odds ratio"
)

implemented_stats_aliases <- tibble::tribble(
  ~ alias, ~ target,
  # Allow case insensitive stat names
  "f",     "F",
  "chisq", "Chisq"
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

  if (is.null(var)) {
    ""
  } else if (inherits(var, "numeric")) {
    "num"
  } else if (length(unique(var)) == 2) {
    "bin"
  } else {
    "mult"
  }
}

# Argument checking --------------------------------------------------------

check_order <- function(x, order, in_calculate = TRUE, stat, call = caller_env()) {
  # If there doesn't need to be an order argument, warn if there is one,
  # and otherwise, skip checks
  if (!(theory_type(x) %in% c("Two sample props z", "Two sample t") ||
        is.null(stat) ||
        stat %in% c("diff in means", "diff in medians",
                    "diff in props", "ratio of props", "odds ratio"))) {
    if (!is.null(order)) {
       warn(paste0(
        "Statistic is not based on a difference or ratio; the `order` argument ",
        "will be ignored. Check `?calculate` for details."
      ))
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
    warn(glue(
      "The statistic is based on a difference or ratio; by default, for ",
      "difference-based statistics, the explanatory variable is subtracted ",
      "in the order \"{unique_ex[1]}\" - \"{unique_ex[2]}\", or divided in ",
      "the order \"{unique_ex[1]}\" / \"{unique_ex[2]}\" for ratio-based ",
      "statistics. To specify this order yourself, supply `order = ",
      "c(\"{unique_ex[1]}\", \"{unique_ex[2]}\")` to the calculate() function."
    ))
  } else if (is.null(order)) {
    order <- as.character(unique_ex)
    warn(glue(
      "The statistic is based on a difference or ratio; by default, for ",
      "difference-based statistics, the explanatory variable is subtracted ",
      "in the order \"{unique_ex[1]}\" - \"{unique_ex[2]}\", or divided in ",
      "the order \"{unique_ex[1]}\" / \"{unique_ex[2]}\" for ratio-based ",
      "statistics. To specify this order yourself, supply `order = ",
      "c(\"{unique_ex[1]}\", \"{unique_ex[2]}\")`."
    ))
  } else {
    if (xor(is.na(order[1]), is.na(order[2]))) {
       abort(paste0(
        "Only one level specified in `order`. Both levels need to be specified."
      ), call = call)
    }
    if (length(order) > 2) {
       abort(paste0("`order` is expecting only two entries."), call = call)
    }
    if (order[1] %in% unique_ex == FALSE) {
       abort(glue("{order[1]} is not a level of the explanatory variable."),
             call = call)
    }
    if (order[2] %in% unique_ex == FALSE) {
       abort(glue("{order[2]} is not a level of the explanatory variable."),
             call = call)
    }
  }
  # return the order as given (unless the argument was invalid or NULL)
  order
}

check_point_params <- function(x, stat, call = caller_env()) {
  param_names <- attr(attr(x, "params"), "names")
  hyp_text <- 'to be set in `hypothesize()`.'
  if (is_hypothesized(x) && !identical(attr(x, "null"), "paired independence")) {
    if (stat %in% c("mean", "median", "sd", "prop")) {
      if ((stat == "mean") && !("mu" %in% param_names)) {
         abort(glue('`stat == "mean"` requires `"mu"` {hyp_text}'), call = call)
      }
      if (!(stat == "mean") && ("mu" %in% param_names)) {
         abort(glue('`"mu"` does not correspond to `stat = "{stat}"`.'), call = call)
      }
      if ((stat == "median") && !("med" %in% param_names)) {
         abort(glue('`stat == "median"` requires `"med"` {hyp_text}'), call = call)
      }
      if (!(stat == "median") && ("med" %in% param_names)) {
         abort(glue('`"med"` does not correspond to `stat = "{stat}"`.'), call = call)
      }
    }
  }
}

# This function checks for NaNs in the output of `calculate` and raises
# a message/warning/error depending on the context in which it was called.
check_for_nan <- function(x, context) {
  if (inherits(x, "infer_dist")) {
    return(x)
  }

  stat_is_nan <- is.nan(x[["stat"]])
  num_nans <- sum(stat_is_nan)
  # If there are no NaNs, continue on as normal :-)
  if (num_nans == 0) {
    return(x)
  }

  calc_ref <- "See ?calculate for more details"
  # If all of the data is NaN, raise an error
  if (num_nans == nrow(x)) {
     abort(glue("All calculated statistics were `NaN`. {calc_ref}."),
           call = NULL)
  }

  stats_were <- if (num_nans == 1) {"statistic was"} else {"statistics were"}
  num_nans_msg <- glue::glue("{num_nans} calculated {stats_were} `NaN`")

  if (context == "visualize") {
    # Raise a warning and plot the data with NaNs removed
     warn(glue(
      "{num_nans_msg}. `NaN`s have been omitted from visualization. {calc_ref}."
    ))
    return(x[!stat_is_nan, ])
  } else if (context == "get_p_value") {
    # Raise an error
    abort(glue(
      "{num_nans_msg}. Simulation-based p-values are not well-defined for ",
      "null distributions with non-finite values. {calc_ref}."
    ), call = NULL)
  }
}

check_direction <- function(direction = c("less", "greater", "two_sided",
                                          "left", "right", "both",
                                          "two-sided", "two sided",
                                          "two.sided"), call = caller_env()) {
  check_type(direction, is.character, call = call)

  if (
    !(direction %in% c("less", "greater", "two_sided", "left", "right",
                       "both", "two-sided", "two sided", "two.sided"))
  ) {
     abort(paste0(
      'The provided value for `direction` is not appropriate. Possible values ',
      'are "less", "greater", "two-sided", "left", "right", "both", ',
      '"two_sided", "two sided", or "two.sided".'
    ), call = call)
  }
}

check_obs_stat <- function(obs_stat, plot = NULL, call = caller_env()) {
  if (!is.null(obs_stat)) {

    if ("data.frame" %in% class(obs_stat)) {
      if (is_fitted(obs_stat)) {
        x_lab <- x_axis_label(plot)

        obs_stat <-
          obs_stat %>%
          dplyr::filter(term == x_lab) %>%
          dplyr::pull(estimate)

        return(obs_stat)
      }

      check_type(obs_stat, is.data.frame, call = call)
      if ((nrow(obs_stat) != 1) || (ncol(obs_stat) != 1)) {
         warn(glue(
          "The first row and first column value of the given `obs_stat` will ",
          "be used."
        ))
      }

      # [[1]] is used in case `stat` is not specified as name of 1x1
      obs_stat <- obs_stat[[1]][[1]]
      check_type(obs_stat, is.numeric, call = call)
    } else {
      check_type(obs_stat, is.numeric, call = call)
    }
  }

  obs_stat
}

check_mlr_x_and_obs_stat <- function(x, obs_stat, fn, arg, call = caller_env()) {
  if (!is_fitted(obs_stat)) {
     abort(glue(
      "The `{arg}` argument should be the output of `fit()`. ",
      "See the documentation with `?{fn}`."
    ), call = call)
  }

  if (!is_generated(x)) {
     abort(paste0(
      "The `x` argument needs to be passed to `generate()` ",
      "before `fit()`."
    ), call = call)
  }

  if (any(!unique(x$term) %in% unique(obs_stat$term)) ||
      any(!unique(obs_stat$term) %in% unique(x$term))) {
     abort(paste0(
      "The explanatory variables used to generate the distribution of ",
      "null fits are not the same used to fit the observed data."
    ), call = call)
  }

  if (response_name(x) != response_name(obs_stat)) {
     abort(glue(
      "The response variable of the null fits ({response_name(x)}) is not ",
      "the same as that of the observed fit ({response_name(obs_stat)})."
    ), call = call)
  }

  invisible(TRUE)
}

#' Check object type
#'
#' Throw an error in case object is not of desired type.
#'
#' @param x An object to check.
#' @param predicate A function to perform check or a formula (as input for
#'   `rlang::as_function()`). A good idea is to use function named `is.*()` or
#'   `is_*()` with possible `<package>::` prefix.
#' @param type_name A string for desired type name. If `NULL`, type is taken
#'   from parsing original name of supplied `predicate`: all alphanumeric with
#'   '_' and '.' characters (until the name end) after the first appearance of
#'   either `is.` or `is_`. In case of a doubt supply `type_name` explicitly.
#' @param x_name String to be used as variable name instead of supplied one
#'   (default).
#' @param allow_null If `TRUE` then error isn't thrown if `x` is `NULL`, no
#'   matter what `predicate(x)` returns.
#' @param ... Arguments to be passed to `predicate`.
#'
#' @examples
#' \donttest{
#' x <- 1
#' check_type(x, is.numeric)
#' check_type(x, is.logical)
#' check_type(x, rlang::is_string, "character of length 1")
#' check_type(
#'   x,
#'   ~ is.character(.) && (length(.) == 1),
#'   "character of length 1"
#' )
#' }
#'
#' @keywords internal
#' @noRd
check_type <- function(x, predicate, type_name = NULL, x_name = NULL,
                        allow_null = FALSE, ..., call = caller_env()) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }

  if (is.null(type_name)) {
    predicate_name <- deparse(rlang::enexpr(predicate))
    type_name <- parse_type(predicate_name)
  }

  predicate <- rlang::as_function(predicate)

  is_pred_true <- (allow_null && is.null(x)) || isTRUE(predicate(x, ...))

  if (!is_pred_true) {
    # Not using "must be of type" because of 'tibble' and 'string' cases
     abort(glue("`{x_name}` must be '{type_name}', not '{get_type(x)}'."),
           call = call)
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
  res <- regmatches(
    f_name,
    regexec("is[_\\.]([[:alnum:]_\\.]+)$", f_name)
  )[[1]][2]

  if (is.na(res)) {
    res <- f_name
  }

  res
}

check_is_distribution <- function(x, fn, call = caller_env()) {
  if (!any(inherits(x, "infer_dist") || is.data.frame(x))) {
     abort(glue(
      "The `x` argument to `{fn}()` must be an infer distribution, ",
      "outputted by `assume()` or `calculate()`."
    ), call = call)
  }
}
