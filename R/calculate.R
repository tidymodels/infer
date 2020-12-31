#' Calculate summary statistics
#'
#' @description
#'
#' Calculates summary statistics from outputs of [generate()] or
#' [hypothesize()].
#'
#' Learn more in `vignette("infer")`.
#'
#' @param x The output from [generate()] for computation-based inference or the
#'   output from [hypothesize()] piped in to here for theory-based inference.
#' @param stat A string giving the type of the statistic to calculate. Current
#'   options include `"mean"`, `"median"`, `"sum"`, `"sd"`, `"prop"`, `"count"`,
#'   `"diff in means"`, `"diff in medians"`, `"diff in props"`, `"Chisq"`,
#'   `"F"`, `"t"`, `"z"`, `"ratio of props"`, `"slope"`, 
#'   `"odds ratio"`, and `"correlation"`.
#' @param order A string vector of specifying the order in which the levels of
#'   the explanatory variable should be ordered for subtraction, where `order =
#'   c("first", "second")` means `("first" - "second")` Needed for inference on
#'   difference in means, medians, or proportions and t and z statistics.
#' @param ... To pass options like `na.rm = TRUE` into functions like
#'   [mean()][base::mean()], [sd()][stats::sd()], etc.
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
#' # calculate a null distribution assuming independence between age
#' # of respondent and whether they have a college degree
#' gss %>%
#'   specify(age ~ college) %>%
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 200, type = "permute") %>%
#'   calculate("diff in means", order = c("degree", "no degree"))
#'
#' # More in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#'
#' @importFrom dplyr group_by summarize n
#' @importFrom rlang !! sym quo enquo eval_tidy
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
  check_type(stat, rlang::is_string)
  check_variables_vs_stat(x, stat)
  check_point_params(x, stat)

  if (!is_generated(x)) {
    x$replicate <- 1L
  }
  
  if (!stat %in% implemented_stats) {
    stop_glue(
      "You specified a string for `stat` that is not implemented. ",
      "Check your spelling and `?calculate` for current options."
    )
  }
  
  if (!is_generated(x) && is_hypothesized(x) && stat %in% untheorized_stats) {
    # this probably ought to be a warning along the lines of having supplied
    # too much information that won't be used
    stop_glue(
      "Theoretical distributions do not exist (or have not been ",
      "implemented) for `stat` = \"{stat}\". Are you missing ",
      "a `generate()` step or mistakenly including ",
      "a `hypothesize()` step?"
    )
  }

  if (stat %in% c("diff in means", "diff in medians", 
                  "diff in props", "ratio of props", "odds ratio") ||
      attr(x, "theory_type") %in% c("Two sample props z", "Two sample t")) {
    order <- check_order(x, explanatory_variable(x), order)
  } else if (!is.null(order)) {
    warning_glue(
      "Statistic is not based on a difference or ratio; the `order` argument",
      " will be ignored. Check `?calculate` for details."
    )
  }
  
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

# Raise an error if the user supplies a test statistic that doesn't
# make sense given the variable specified
check_variables_vs_stat <- function(x, stat) {
  res_type <- determine_variable_type(x, "response")
  exp_type <- determine_variable_type(x, "explanatory")
  
  possible_stats <- stat_types %>%
    dplyr::filter(resp == res_type & exp == exp_type) %>%
    dplyr::pull(stats) %>%
    unlist()

  if (is.null(possible_stats)) {
    stop_glue(
      "The infer team has not implemented test statistics for the ",
      "supplied variable types."
    )
  }
  
  if (!stat %in% possible_stats) {
    stop_glue(
      stat_desc$description[stat_desc$stat == stat],
      ' is not well-defined for a ',
      stat_type_desc$description[
        determine_variable_type(x, "response") == stat_type_desc$type
      ],
      " response variable ({as.character(attr(x, 'response'))}) and ",
      if (has_explanatory(x)) {
        glue_null("a ", stat_type_desc$description[
          determine_variable_type(x, "explanatory") == stat_type_desc$type
        ], " explanatory variable ({as.character(attr(x, 'explanatory'))}).")
      } else {
        "no explanatory variable."
      }
    )
  }
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

    if (attr_is_null(x, "success")) {
      stop_glue(
        'To calculate a {output_name}, the `"success"` argument must be ',
        "provided in `specify()`."
      )
    }

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
        stats::lm(!!(attr(x, "response")) ~ !!(attr(x, "explanatory")))
      )$`F value`[1]
    )
}

calc_impl.slope <- function(type, x, order, ...) {
  x %>%
    dplyr::summarize(
      stat = stats::coef(
        stats::lm(!!(attr(x, "response")) ~ !!(attr(x, "explanatory")))
      )[2]
    )
}

calc_impl.correlation <- function(type, x, order, ...) {
  x %>%
    dplyr::summarize(
      stat = stats::cor(!!attr(x, "explanatory"), !!attr(x, "response"))
    )
}

calc_impl_diff_f <- function(f) {
  function(type, x, order, ...) {
    x %>%
      dplyr::group_by(replicate, !!attr(x, "explanatory"), .drop = FALSE) %>%
      dplyr::summarize(value = f(!!attr(x, "response"), ...)) %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(
        stat = value[!!(attr(x, "explanatory")) == order[1]] -
          value[!!(attr(x, "explanatory")) == order[2]]
      )
  }
}

calc_impl.diff_in_means <- calc_impl_diff_f(mean)

calc_impl.diff_in_medians <- calc_impl_diff_f(stats::median)

calc_impl.Chisq <- function(type, x, order, ...) {
  resp_var <- as.character(attr(x, "response"))

  if (attr_is_null(x, "explanatory")) {
    # Chi-Square Goodness of Fit
    if (!attr_is_null(x, "params")) {
      # When `hypothesize()` has been called
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
      # Straight from `specify()`
      stop_glue(
        "In order to calculate a Chi-Square Goodness of Fit statistic, ",
        "hypothesized values must be given for the `p` parameter in the ",
        "`hypothesize()` function prior to using `calculate()`"
      )
    }
  } else {
    # Chi-Square Test of Independence
    expl_var <- as.character(attr(x, "explanatory"))
    chisq_indep <- function(df) {
      res <- suppressWarnings(stats::chisq.test(
        x = df[[expl_var]],
        y = df[[resp_var]]
      ))

      res[["statistic"]]
    }

    # Warn about possible unused factor levels
    if (has_unused_levels(x[[expl_var]])) {
      warning_glue("Explanatory variable has unused factor levels.")
    }
    if (has_unused_levels(x[[resp_var]])) {
      warning_glue("Response variable has unused factor levels.")
    }

    # Compute result
    result <- x %>%
      dplyr::nest_by(.key = "data") %>%
      dplyr::summarise(stat = chisq_indep(data), .groups = "drop")
  }

  if (!attr_is_null(x, "generated")) {
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
  col <- attr(x, "response")
  success <- attr(x, "success")

  x %>%
    dplyr::group_by(replicate, !!attr(x, "explanatory"), .drop = FALSE) %>%
    dplyr::summarize(prop = mean(!!sym(col) == success, ...)) %>%
    dplyr::summarize(
      stat = operator(
        prop[!!attr(x, "explanatory") == order[1]],
        prop[!!attr(x, "explanatory") == order[2]]
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
  col <- attr(x, "response")
  success <- attr(x, "success")

  x %>%
    dplyr::group_by(replicate, !!attr(x, "explanatory"), .drop = FALSE) %>%
    dplyr::summarize(prop = mean(!!sym(col) == success, ...)) %>%
    dplyr::summarize(
      prop_1 = prop[!!attr(x, "explanatory") == order[1]],
      prop_2 = prop[!!attr(x, "explanatory") == order[2]],
      stat = (prop_1 / prop_2) / ((1 - prop_1) / (1 - prop_2))
    ) %>%
    dplyr::select(stat)
}

calc_impl.t <- function(type, x, order, ...) {
  if (attr(x, "theory_type") == "Two sample t") {
    x <- reorder_explanatory(x, order)

    df_out <- x %>%
      dplyr::summarize(
        stat = stats::t.test(
          !!attr(x, "response") ~ !!attr(x, "explanatory"), ...
        )[["statistic"]]
      )
  } else if (attr(x, "theory_type") == "One sample t") {
    if (!is_hypothesized(x)) {
      # For bootstrap
      if (is.null(list(...)$mu)) {
        message_glue(
          "No `mu` argument was hypothesized, so the t-test will ",
          "assume a null hypothesis `mu = 0`."
        )
      }

      df_out <- x %>%
        dplyr::summarize(
          stat = stats::t.test(!!attr(x, "response"), ...)[["statistic"]]
        )
    } else {
      # For hypothesis testing
      df_out <- x %>%
        dplyr::summarize(
          stat = stats::t.test(
            !!attr(x, "response"),
            mu = attr(x, "params"),
            ...
          )[["statistic"]]
        )
    }
  }
  df_out
}

calc_impl.z <- function(type, x, order, ...) {
  # Two sample proportions
  if (attr(x, "theory_type") == "Two sample props z") {
    col <- attr(x, "response")
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
  } else if (attr(x, "theory_type") == "One sample prop z") {
    # One sample proportion

    # When `hypothesize()` has been called
    success <- attr(x, "success")
    col <- attr(x, "response")
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
