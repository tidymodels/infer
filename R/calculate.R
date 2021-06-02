#' Calculate summary statistics
#'
#' @description
#'
#' Given the output of [specify()] and/or [hypothesize()], this function will
#' return the observed statistic specified with the `stat` argument. Some test
#' statistics, such as `Chisq`, `t`, and `z`, require a null hypothesis. If
#' provided the output of [generate()], the function will calculate the
#' supplied `stat` for each `replicate`.
#'
#' Learn more in `vignette("infer")`.
#'
#' @param x The output from [generate()] for computation-based inference or the
#'   output from [hypothesize()] piped in to here for theory-based inference.
#' @param stat A string giving the type of the statistic to calculate. Current
#'   options include `"mean"`, `"median"`, `"sum"`, `"sd"`, `"prop"`, `"count"`,
#'   `"diff in means"`, `"diff in medians"`, `"diff in props"`, `"Chisq"` (or
#'   `"chisq"`), `"F"` (or `"f"`), `"t"`, `"z"`, `"ratio of props"`, `"slope"`,
#'   `"odds ratio"`, and `"correlation"`.
#' @param order A string vector of specifying the order in which the levels of
#'   the explanatory variable should be ordered for subtraction (or division
#'   for ratio-based statistics), where `order = c("first", "second")` means 
#'   `("first" - "second")`, or the analogue for ratios. Needed for inference on
#'   difference in means, medians, proportions, ratios, t, and z statistics.
#' @param ... To pass options like `na.rm = TRUE` into functions like
#'   [mean()][base::mean()], [sd()][stats::sd()], etc. Can also be used to
#'   supply hypothesized null values for the `"t"` statistic.
#'
#' @return A tibble containing a `stat` column of calculated statistics.
#'
#' @section Missing levels in small samples:
#' In some cases, when bootstrapping with small samples, some generated
#' bootstrap samples will have only one level of the explanatory variable
#' present. For some test statistics, the calculated statistic in these
#' cases will be NaN. The package will omit non-finite values from
#' visualizations (with a warning) and raise an error in p-value calculations.
#'
#' @examples
#'
#' # calculate a null distribution of hours worked per week under
#' # the null hypothesis that the mean is 40
#' gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40) %>%
#'   generate(reps = 200, type = "bootstrap") %>%
#'   calculate(stat = "mean")
#'   
#' # calculate the corresponding observed statistic
#' gss %>%
#'   specify(response = hours) %>%
#'   calculate(stat = "mean")
#'
#' # calculate a null distribution assuming independence between age
#' # of respondent and whether they have a college degree
#' gss %>%
#'   specify(age ~ college) %>%
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 200, type = "permute") %>%
#'   calculate("diff in means", order = c("degree", "no degree"))
#'   
#' # calculate the corresponding observed statistic
#' gss %>%
#'   specify(age ~ college) %>%
#'   calculate("diff in means", order = c("degree", "no degree"))
#'   
#' # some statistics require a null hypothesis
#'  gss %>%
#'    specify(response = hours) %>% 
#'    hypothesize(null = "point", mu = 40) %>%
#'    calculate(stat = "t")
#'    
#' # More in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @seealso [visualize()], [get_p_value()], and [get_confidence_interval()]
#' to extract value from this function's outputs.
#'
#' @importFrom dplyr group_by summarize n
#' @importFrom rlang !! sym quo enquo eval_tidy
#' @family core functions
#' @export
calculate <- function(x,
                      stat = c(
                        "mean", "median", "sum", "sd", "prop", "count",
                        "diff in means", "diff in medians", "diff in props",
                        "Chisq", "F", "slope", "correlation", "t", "z",
                        "ratio of props", "odds ratio"
                      ),
                      order = NULL,
                      ...) {
  check_type(x, tibble::is_tibble)
  check_if_mlr(x)
  stat <- check_calculate_stat(stat)
  check_variables_vs_stat(x, stat)
  check_point_params(x, stat)
  
  order <- check_order(x, order, in_calculate = TRUE, stat)

  if (!is_generated(x)) {
    x$replicate <- 1L
  }
  
  x <- message_on_excessive_null(x, stat)
  x <- warn_on_insufficient_null(x, stat, ...)
  
  # Use S3 method to match correct calculation
  result <- calc_impl(
    structure(stat, class = gsub(" ", "_", stat)), x, order, ...
  )

  result <- copy_attrs(to = result, from = x)
  attr(result, "stat") <- stat

  # For returning a 1x1 observed statistic value
  if (nrow(result) == 1) {
    result <- select(result, stat)
  }

  result
}

check_if_mlr <- function(x) {
  if (is_mlr(x)) {
    stop_glue("Multiple explanatory variables are not supported in calculate().")
  }
}

check_calculate_stat <- function(stat) {
  
  check_type(stat, rlang::is_string)
  
  # Check for possible `stat` aliases
  alias_match_id <- match(stat, implemented_stats_aliases[["alias"]])
  if (!is.na(alias_match_id)) {
    stat <- implemented_stats_aliases[["target"]][alias_match_id]
  } else {
    rlang::arg_match(stat, implemented_stats)
  }

  stat
}

# Raise an error if the user supplies a test statistic that doesn't
# make sense given the variable specified
check_variables_vs_stat <- function(x, stat) {
  response_type <- determine_variable_type(x, "response")
  explanatory_type <- determine_variable_type(x, "explanatory")

  possible_stats <- stat_types %>%
    dplyr::filter(resp == response_type & exp == explanatory_type) %>%
    dplyr::pull(stats) %>%
    unlist()

  if (is.null(possible_stats)) {
    stop_glue(
      "The infer team has not implemented test statistics for the ",
      "supplied variable types."
    )
  }

  if (!stat %in% possible_stats) {
    if (has_explanatory(x)) {
      msg_tail <- glue_null(
        "a {get_stat_type_desc(explanatory_type)} explanatory variable ",
        "({explanatory_name(x)})."
      )
    } else {
      msg_tail <- "no explanatory variable."
    }

    stop_glue(
      "{get_stat_desc(stat)} is not well-defined for a ",
      "{get_stat_type_desc(response_type)} response variable ",
      "({response_name(x)}) and ", msg_tail
    )
  }
}

# When given no hypothesis for a theorized statistic, supply a reasonable value
assume_null <- function(x, stat_) {
  null_fn <- theorized_nulls %>%
    dplyr::filter(stat == stat_) %>%
    dplyr::pull(null_fn) %>%
    `[[`(1)
  
  null_fn(x)
}


# User supplied "too much" information - hypothesized a value for a point
# estimate that isn't relevant to the statistic calculation
message_on_excessive_null <- function(x, stat) {
  if (!is_generated(x) && is_hypothesized(x) && stat %in% untheorized_stats) {
    null_type <- attr(x, "null")
    null_param <- attr(x, "params")
    
    message_glue(
      "Message: The {null_type} null hypothesis ",
      "`{names(null_param)} = {unname(null_param)}`",
      " does not inform calculation of the observed statistic (",
      "{tolower(get_stat_desc(stat))}) and will ",
      "be ignored."
    )
  }
  
  x
}

# User didn't supply "enough" information - no hypothesis for a theorized
# statistic on a point estimate, so warn that a reasonable value was assumed.
warn_on_insufficient_null <- function(x, stat, ...) {
  if (!is_hypothesized(x)          && 
      !has_explanatory(x)          && 
      !stat %in% untheorized_stats &&
      !(stat == "t" && "mu" %in% names(list(...)))) {
    attr(x, "null") <- "point"
    attr(x, "params") <- assume_null(x, stat)
    
    warning_glue(
      "{get_stat_desc(stat)} requires a null ",
      "hypothesis to calculate the observed statistic. \nOutput assumes ",
      "the following null value{print_params(x)}."
    )
  } 
  
  x
}

calc_impl <- function(type, x, order, ...) {
  UseMethod("calc_impl", type)
}

calc_impl_one_f <- function(f) {
  function(type, x, order, ...) {
    col <- base::setdiff(names(x), "replicate")

    x %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = f(!!(sym(col)), ...))
  }
}

calc_impl.mean <- calc_impl_one_f(mean)

calc_impl.median <- calc_impl_one_f(stats::median)

calc_impl.sum <- calc_impl_one_f(sum)

calc_impl.sd <- calc_impl_one_f(stats::sd)

calc_impl_success_f <- function(f, output_name) {
  function(type, x, order, ...) {
    col <- base::setdiff(names(x), "replicate")

    success <- attr(x, "success")
    x %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = f(!!sym(col), success))
  }
}

calc_impl.prop <- calc_impl_success_f(
  f = function(response, success, ...) {
    mean(response == success, ...)
  },
  output_name = "proportion"
)

calc_impl.count <- calc_impl_success_f(
  f = function(response, success, ...) {
    sum(response == success, ...)
  },
  output_name = "count"
)

calc_impl.F <- function(type, x, order, ...) {
  x %>%
    dplyr::summarize(
      stat = stats::anova(
        stats::lm(!!(response_expr(x)) ~ !!(explanatory_expr(x)))
      )$`F value`[1]
    )
}

calc_impl.slope <- function(type, x, order, ...) {
  x %>%
    dplyr::summarize(
      stat = stats::coef(
        stats::lm(!!(response_expr(x)) ~ !!(explanatory_expr(x)))
      )[2]
    )
}

calc_impl.correlation <- function(type, x, order, ...) {
  x %>%
    dplyr::summarize(
      stat = stats::cor(!!explanatory_expr(x), !!response_expr(x))
    )
}

calc_impl_diff_f <- function(f) {
  function(type, x, order, ...) {
    x %>%
      dplyr::group_by(replicate, !!explanatory_expr(x), .drop = FALSE) %>%
      dplyr::summarize(value = f(!!response_expr(x), ...)) %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(
        stat = value[!!(explanatory_expr(x)) == order[1]] -
          value[!!(explanatory_expr(x)) == order[2]]
      )
  }
}

calc_impl.diff_in_means <- calc_impl_diff_f(mean)

calc_impl.diff_in_medians <- calc_impl_diff_f(stats::median)

calc_impl.Chisq <- function(type, x, order, ...) {
  resp_var <- response_name(x)

  if (!has_attr(x, "explanatory")) {
    # Chi-Square Goodness of Fit
    p_levels <- get_par_levels(x)
    chisq_gof <- function(df) {
      chisq <- suppressWarnings(stats::chisq.test(
        # Ensure correct ordering of parameters
        table(df[[resp_var]])[p_levels],
        p = attr(x, "params")
      ))
      
      unname(chisq[["statistic"]])
    }
    
    result <- x %>%
      dplyr::nest_by(.key = "data") %>%
      dplyr::summarise(stat = chisq_gof(data), .groups = "drop")
  } else {
    # Chi-Square Test of Independence
    expl_var <- explanatory_name(x)
    chisq_indep <- function(df) {
      res <- suppressWarnings(stats::chisq.test(
        x = df[[expl_var]],
        y = df[[resp_var]]
      ))

      res[["statistic"]]
    }

    # Compute result
    result <- x %>%
      dplyr::nest_by(.key = "data") %>%
      dplyr::summarise(stat = chisq_indep(data), .groups = "drop")
  }

  if (is_generated(x)) {
    result <- result %>% dplyr::select(replicate, stat)
  } else {
    result <- result %>% dplyr::select(stat)
  }

  copy_attrs(
    to = result, from = x,
    attrs = c(
      "response", "success", "explanatory", "response_type",
      "explanatory_type", "distr_param", "distr_param2", "theory_type"
    )
  )
}

calc_impl.function_of_props <- function(type, x, order, operator, ...) {
  col <- response_expr(x)
  success <- attr(x, "success")

  x %>%
    dplyr::group_by(replicate, !!explanatory_expr(x), .drop = FALSE) %>%
    dplyr::summarize(prop = mean(!!sym(col) == success, ...)) %>%
    dplyr::summarize(
      stat = operator(
        prop[!!explanatory_expr(x) == order[1]],
        prop[!!explanatory_expr(x) == order[2]]
      )
    )
}

calc_impl.diff_in_props <- function(type, x, order, ...) {
  calc_impl.function_of_props(type, x, order, operator = `-`, ...)
}

calc_impl.ratio_of_props <- function(type, x, order, ...) {
  calc_impl.function_of_props(type, x, order, operator = `/`, ...)
}

calc_impl.odds_ratio <- function(type, x, order, ...) {
  col <- response_expr(x)
  success <- attr(x, "success")

  x %>%
    dplyr::group_by(replicate, !!explanatory_expr(x), .drop = FALSE) %>%
    dplyr::summarize(prop = mean(!!sym(col) == success, ...)) %>%
    dplyr::summarize(
      prop_1 = prop[!!explanatory_expr(x) == order[1]],
      prop_2 = prop[!!explanatory_expr(x) == order[2]],
      stat = (prop_1 / prop_2) / ((1 - prop_1) / (1 - prop_2))
    ) %>%
    dplyr::select(stat)
}

calc_impl.t <- function(type, x, order, ...) {
  if (theory_type(x) == "Two sample t") {
    x <- reorder_explanatory(x, order)

    df_out <- x %>%
      dplyr::summarize(
        stat = stats::t.test(
          !!response_expr(x) ~ !!explanatory_expr(x), ...
        )[["statistic"]]
      )
  } else if (theory_type(x) == "One sample t") {
    if (!is_hypothesized(x)) {
      # For bootstrap
      df_out <- x %>%
        dplyr::summarize(
          stat = stats::t.test(!!response_expr(x), ...)[["statistic"]]
        )
    } else {
      # For hypothesis testing
      df_out <- x %>%
        dplyr::summarize(
          stat = stats::t.test(
            !!response_expr(x),
            mu = attr(!!x, "params"),
            ...
          )[["statistic"]]
        )
    }
  }
  df_out
}

calc_impl.z <- function(type, x, order, ...) {
  # Two sample proportions
  if (theory_type(x) == "Two sample props z") {
    col <- response_expr(x)
    success <- attr(x, "success")

    x$explan <- factor(
      explanatory_variable(x),
      levels = c(order[1], order[2])
    )

    aggregated <- x %>%
      dplyr::group_by(replicate, explan) %>%
      dplyr::summarize(
        group_num = dplyr::n(),
        prop = mean(rlang::eval_tidy(col) == rlang::eval_tidy(success)),
        num_suc = sum(rlang::eval_tidy(col) == rlang::eval_tidy(success))
      )

    df_out <- aggregated %>%
      dplyr::summarize(
        diff_prop = prop[explan == order[1]] - prop[explan == order[2]],
        total_suc = sum(num_suc),
        n1 = group_num[1],
        n2 = group_num[2],
        p_hat = total_suc / (n1 + n2),
        denom = sqrt(p_hat * (1 - p_hat) / n1 + p_hat * (1 - p_hat) / n2),
        stat = diff_prop / denom
      ) %>%
      dplyr::select(stat)

    df_out
  } else if (theory_type(x) == "One sample prop z") {
    # One sample proportion

    # When `hypothesize()` has been called
    success <- attr(x, "success")
    col <- response_expr(x)
    p0 <- unname(attr(x, "params")[1])
    num_rows <- nrow(x) / length(unique(x$replicate))
      
    df_out <- x %>%
      dplyr::summarize(
        stat = (
          mean(rlang::eval_tidy(col) == rlang::eval_tidy(success), ...) - p0
        ) / sqrt((p0 * (1 - p0)) / num_rows)
      )

    df_out
  }
}
