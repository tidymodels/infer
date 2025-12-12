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
#' @param stat A string giving the type of the statistic to calculate or a
#'   function that takes in a replicate of `x` and returns a scalar value. Current
#'   options include `"mean"`, `"median"`, `"sum"`, `"sd"`, `"prop"`, `"count"`,
#'   `"diff in means"`, `"diff in medians"`, `"diff in props"`, `"Chisq"` (or
#'   `"chisq"`), `"F"` (or `"f"`), `"t"`, `"z"`, `"ratio of props"`, `"slope"`,
#'   `"odds ratio"`, `"ratio of means"`, and `"correlation"`. `infer` only
#'   supports theoretical tests on one or two means via the `"t"` distribution
#'   and one or two proportions via the `"z"`. See the "Arbitrary test statistics"
#'   section below for more on how to define a custom statistic.
#' @param order A string vector of specifying the order in which the levels of
#'   the explanatory variable should be ordered for subtraction (or division
#'   for ratio-based statistics), where `order = c("first", "second")` means
#'   `("first" - "second")`, or the analogue for ratios. Needed for inference on
#'   difference in means, medians, proportions, ratios, t, and z statistics.
#' @param ... To pass options like `na.rm = TRUE` into functions like
#'   [mean()][base::mean()], [sd()][stats::sd()], etc. Can also be used to
#'   supply hypothesized null values for the `"t"` statistic or additional
#'   arguments to [stats::chisq.test()].
#'
#' @return A tibble containing a `stat` column of calculated statistics.
#'
#' @section Arbitrary test statistics:
#'
#' In addition to the pre-implemented statistics documented in `stat`, users can
#' supply an arbitrary test statistic by supplying a function to the `stat`
#' argument.
#'
#' The function should have arguments `stat(x, order, ...)`, where `x` is one
#' replicate's worth of `x`. The `order` argument and ellipses will be supplied
#' directly to the `stat` function. Internally, `calculate()` will split `x` up
#' into data frames by replicate and pass them one-by-one to the supplied `stat`.
#' For example, to implement `stat = "mean"` as a function, one could write:
#'
#' ```r
#' stat_mean <- function(x, order, ...) {mean(x$hours)}
#' obs_mean <-
#'   gss %>%
#'   specify(response = hours) %>%
#'   calculate(stat = stat_mean)
#'
#' set.seed(1)
#' null_dist_mean <-
#'   gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40) %>%
#'   generate(reps = 5, type = "bootstrap") %>%
#'   calculate(stat = stat_mean)
#' ```
#'
#' Note that the same `stat_mean` function is supplied to both `generate()`d and
#' non-`generate()`d infer objects--no need to implement support for grouping
#' by `replicate` yourself.
#'
#' @section Missing levels in small samples:
#' In some cases, when bootstrapping with small samples, some generated
#' bootstrap samples will have only one level of the explanatory variable
#' present. For some test statistics, the calculated statistic in these
#' cases will be NaN. The package will omit non-finite values from
#' visualizations (with a warning) and raise an error in p-value calculations.
#'
#' @includeRmd man-roxygen/seeds.Rmd
#'
#' @examples
#'
#' # calculate a null distribution of hours worked per week under
#' # the null hypothesis that the mean is 40
#' gss |>
#'   specify(response = hours) |>
#'   hypothesize(null = "point", mu = 40) |>
#'   generate(reps = 200, type = "bootstrap") |>
#'   calculate(stat = "mean")
#'
#' # calculate the corresponding observed statistic
#' gss |>
#'   specify(response = hours) |>
#'   calculate(stat = "mean")
#'
#' # calculate a null distribution assuming independence between age
#' # of respondent and whether they have a college degree
#' gss |>
#'   specify(age ~ college) |>
#'   hypothesize(null = "independence") |>
#'   generate(reps = 200, type = "permute") |>
#'   calculate("diff in means", order = c("degree", "no degree"))
#'
#' # calculate the corresponding observed statistic
#' gss |>
#'   specify(age ~ college) |>
#'   calculate("diff in means", order = c("degree", "no degree"))
#'
#' # some statistics require a null hypothesis
#'  gss |>
#'    specify(response = hours) |>
#'    hypothesize(null = "point", mu = 40) |>
#'    calculate(stat = "t")
#'
#' # more in-depth explanation of how to use the infer package
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
calculate <- function(
  x,
  stat = c(
    "mean",
    "median",
    "sum",
    "sd",
    "prop",
    "count",
    "diff in means",
    "diff in medians",
    "diff in props",
    "Chisq",
    "F",
    "slope",
    "correlation",
    "t",
    "z",
    "ratio of props",
    "odds ratio",
    "ratio of means"
  ),
  order = NULL,
  ...
) {
  check_type(x, tibble::is_tibble)
  check_if_mlr(x, "calculate")
  stat_chr <- stat_chr(stat)
  stat_chr <- check_calculate_stat(stat_chr)
  check_input_vs_stat(x, stat_chr)
  check_point_params(x, stat_chr)

  order <- check_order(x, order, in_calculate = TRUE, stat_chr)

  if (!is_generated(x)) {
    x$replicate <- 1L
  }

  x <- message_on_excessive_null(x, stat = stat_chr, fn = "calculate")
  x <- warn_on_insufficient_null(x, stat_chr, ...)

  # Use S3 method to match correct calculation
  result <- calc_impl(
    structure(stat, class = gsub(" ", "_", stat_chr)),
    x,
    order,
    ...
  )

  result <- copy_attrs(to = result, from = x)
  attr(result, "stat") <- stat

  # For returning a 1x1 observed statistic value
  if (nrow(result) == 1) {
    result <- select(result, stat)
  }

  append_infer_class(result)
}

check_if_mlr <- function(x, fn, call = caller_env()) {
  if (fn == "calculate") {
    suggestion <-
      "When working with multiple explanatory variables, use \\
        {.help [{.fun fit}](infer::fit.infer)} instead."
  } else {
    suggestion <- ""
  }

  if (is_mlr(x)) {
    cli_abort(
      c(
        "Multiple explanatory variables are not supported in {.fun {fn}}.",
        i = suggestion
      ),
      call = call
    )
  }
}

stat_chr <- function(stat) {
  if (rlang::is_function(stat)) {
    return("function")
  }

  stat
}

check_calculate_stat <- function(stat, call = caller_env()) {
  check_type(stat, rlang::is_string, call = call)
  if (identical(stat, "function")) {
    return(stat)
  }

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
# make sense given the variable and hypothesis specified
check_input_vs_stat <- function(x, stat, call = caller_env()) {
  response_type <- attr(x, "type_desc_response")
  explanatory_type <- attr(x, "type_desc_explanatory")

  possible_stats <- stat_types |>
    dplyr::filter(resp == response_type & exp == explanatory_type) |>
    dplyr::pull(stats) |>
    unlist()

  if (is.null(possible_stats)) {
    cli_abort(
      "The infer team has not implemented test statistics for the \\
       supplied variable types.",
      call = call
    )
  }

  if (identical(stat, "function")) {
     return(x)
  }

  if (!stat %in% possible_stats) {
    if (has_explanatory(x)) {
      msg_tail <- glue(
        "a {get_stat_type_desc(explanatory_type)} explanatory variable ",
        "({explanatory_name(x)}).",
        .null = "NULL"
      )
    } else {
      msg_tail <- "no explanatory variable."
    }

    cli_abort(
      "{get_stat_desc(stat)} is not well-defined for a \\
       {get_stat_type_desc(response_type)} response variable \\
       ({response_name(x)}) and {msg_tail}",
      call = call
    )
  }

  if (is_hypothesized(x)) {
    stat_nulls <- stat_hypotheses |>
      dplyr::filter(
        stat == !!stat &
          hypothesis == attr(x, "null")
      )

    if (nrow(stat_nulls) == 0) {
      cli_abort(
        'The supplied statistic `stat = "{stat}"` is incompatible with the \\
         supplied hypothesis `null = "{attr(x, "null")}"`.',
        call = call
      )
    }
  }

  x
}

# When given no hypothesis for a theorized statistic, supply a reasonable value
.subset_1 <- function(x) {x[[1]]}
assume_null <- function(x, stat_) {
  null_fn <- theorized_nulls |>
    dplyr::filter(stat == stat_) |>
    dplyr::pull(null_fn) |>
    .subset_1()

  null_fn(x)
}


# User supplied "too much" information - hypothesized a value for a point
# estimate that isn't relevant to the statistic calculation
#
# The `stat = "mean"` default ensures that `stat %in% untheorized_stats`
# when called in non-`calculate` functions
message_on_excessive_null <- function(x, stat = "mean", fn) {
  if (!is_generated(x) && is_hypothesized(x) && stat %in% untheorized_stats) {
    null_type <- attr(x, "null")
    null_param <- attr(x, "params")

    cli_inform(
      "Message: The {null_type} null hypothesis \\
       {if (null_type == 'point') {paste0('`', names(null_param), ' = ', unname(null_param), '` ')} else {''}} \\
       does not inform calculation of the observed \\
       {if (fn == 'calculate') {paste0('statistic (', tolower(get_stat_desc(stat)), ') ')} else {'fit '}} \\
       and will be ignored."
    )
  }

  x
}

# User didn't supply "enough" information - no hypothesis for a theorized
# statistic on a point estimate, so warn that a reasonable value was assumed.
warn_on_insufficient_null <- function(x, stat, ...) {
  if (
    !is_hypothesized(x) &&
      !has_explanatory(x) &&
      !stat %in% c(untheorized_stats, "function") &&
      !(stat == "t" && "mu" %in% names(list(...)))
  ) {
    attr(x, "null") <- "point"
    attr(x, "params") <- assume_null(x, stat)

    cli_warn(c(
      "{get_stat_desc(stat)} requires a null \\
       hypothesis to calculate the observed statistic.",
      "Output assumes the following null value{print_params(x)}."
    ))
  }

  x
}

calc_impl <- function(type, x, order, ...) {
  UseMethod("calc_impl", type)
}

calc_impl_one_f <- function(f) {
  function(type, x, order, ...) {
    col <- base::setdiff(names(x), "replicate")

    if (!identical(dplyr::group_vars(x), "replicate")) {
      x <- dplyr::group_by(x, replicate)
    }

    res <- x |>
      dplyr::summarize(stat = f(!!(sym(col)), ...))

    # calculate SE for confidence intervals
    if (!is_generated(x)) {
      sample_sd <- x |>
        dplyr::summarize(stats::sd(!!(sym(col)))) |>
        dplyr::pull()

      attr(res, "se") <- sample_sd / sqrt(nrow(x))
    }

    res
  }
}

#' @export
calc_impl.mean <- calc_impl_one_f(mean)

#' @export
calc_impl.median <- calc_impl_one_f(stats::median)

#' @export
calc_impl.sum <- calc_impl_one_f(sum)

#' @export
calc_impl.sd <- calc_impl_one_f(stats::sd)

calc_impl_success_f <- function(f, output_name) {
  function(type, x, order, ...) {
    col <- base::setdiff(names(x), "replicate")

    success <- attr(x, "success")

    if (!identical(dplyr::group_vars(x), "replicate")) {
      x <- dplyr::group_by(x, replicate)
    }

    res <- x |>
      dplyr::summarize(stat = f(!!sym(col), success))

    # calculate SE for confidence intervals
    if (!is_generated(x) && output_name == "proportion") {
      prop <- res[["stat"]]

      attr(res, "se") <- sqrt((prop * (1 - prop)) / nrow(x))
    }

    res
  }
}

#' @export
calc_impl.prop <- calc_impl_success_f(
  f = function(response, success, ...) {
    mean(response == success, ...)
  },
  output_name = "proportion"
)

#' @export
calc_impl.count <- calc_impl_success_f(
  f = function(response, success, ...) {
    sum(response == success, ...)
  },
  output_name = "count"
)

#' @export
calc_impl.F <- function(type, x, order, ...) {
  x |>
    dplyr::summarize(
      stat = stats::anova(
        stats::lm(!!(response_expr(x)) ~ !!(explanatory_expr(x)))
      )$`F value`[1]
    )
}

#' @export
calc_impl.slope <- function(type, x, order, ...) {
  x |>
    dplyr::summarize(
      stat = stats::coef(
        stats::lm(!!(response_expr(x)) ~ !!(explanatory_expr(x)))
      )[2]
    )
}

#' @export
calc_impl.correlation <- function(type, x, order, ...) {
  x |>
    dplyr::summarize(
      stat = stats::cor(!!explanatory_expr(x), !!response_expr(x))
    )
}

calc_impl_diff_f <- function(f, operator) {
  function(type, x, order, ...) {
    res <- x |>
      dplyr::group_by(replicate, !!explanatory_expr(x), .drop = FALSE) |>
      dplyr::summarize(value = f(!!response_expr(x), ...)) |>
      dplyr::group_by(replicate) |>
      dplyr::summarize(
        stat = operator(
          value[!!(explanatory_expr(x)) == order[1]],
          value[!!(explanatory_expr(x)) == order[2]]
        )
      )

    # calculate SE for confidence intervals
    if (!is_generated(x) && identical(operator, `-`)) {
      sample_sds <- x |>
        dplyr::group_by(replicate, !!explanatory_expr(x), .drop = FALSE) |>
        dplyr::summarize(stats::sd(!!response_expr(x))) |>
        dplyr::pull()

      sample_counts <- x |>
        dplyr::count(!!explanatory_expr(x), .drop = FALSE) |>
        dplyr::pull()

      attr(res, "se") <-
        sqrt(
          sum(
            (sample_sds[1] / sqrt(sample_counts[1]))^2,
            (sample_sds[2] / sqrt(sample_counts[2]))^2
          )
        )
    }

    res
  }
}

#' @export
calc_impl.diff_in_means <- calc_impl_diff_f(mean, operator = `-`)

#' @export
calc_impl.diff_in_medians <- calc_impl_diff_f(stats::median, operator = `-`)

#' @export
calc_impl.ratio_of_means <- calc_impl_diff_f(mean, operator = `/`)

#' @export
calc_impl.Chisq <- function(type, x, order, ...) {
  resp_var <- response_name(x)

  if (!has_attr(x, "explanatory")) {
    # Chi-Square Goodness of Fit
    p_levels <- get_par_levels(x)
    chisq_gof <- function(df) {
      chisq <- suppressWarnings(stats::chisq.test(
        # Ensure correct ordering of parameters
        table(df[[resp_var]])[p_levels],
        p = attr(x, "params"),
        ...
      ))

      unname(chisq[["statistic"]])
    }

    result <- x |>
      dplyr::nest_by(.key = "data") |>
      dplyr::summarise(stat = chisq_gof(data), .groups = "drop")
  } else {
    # Chi-Square Test of Independence
    expl_var <- explanatory_name(x)
    chisq_indep <- function(df) {
      res <- suppressWarnings(stats::chisq.test(
        x = df[[expl_var]],
        y = df[[resp_var]],
        ...
      ))

      res[["statistic"]]
    }

    # Compute result
    result <- x |>
      dplyr::nest_by(.key = "data") |>
      dplyr::summarise(stat = chisq_indep(data), .groups = "drop")
  }

  if (is_generated(x)) {
    result <- result |> dplyr::select(replicate, stat)
  } else {
    result <- result |> dplyr::select(stat)
  }

  copy_attrs(
    to = result,
    from = x,
    attrs = c(
      "response",
      "success",
      "explanatory",
      "response_type",
      "explanatory_type",
      "distr_param",
      "distr_param2",
      "theory_type",
      "type_desc_response",
      "type_desc_explanatory"
    )
  )
}

#' @export
calc_impl.function_of_props <- function(type, x, order, operator, ...) {
  col <- response_expr(x)
  success <- attr(x, "success")

  res <- x |>
    dplyr::group_by(replicate, !!explanatory_expr(x), .drop = FALSE) |>
    dplyr::summarize(prop = mean(!!sym(col) == success, ...)) |>
    dplyr::summarize(
      stat = operator(
        prop[!!explanatory_expr(x) == order[1]],
        prop[!!explanatory_expr(x) == order[2]]
      )
    )

  # calculate SE for confidence intervals
  if (!is_generated(x)) {
    props <- x |>
      dplyr::group_by(!!explanatory_expr(x), .drop = FALSE) |>
      dplyr::summarize(prop = mean(!!sym(col) == success, ...)) |>
      dplyr::pull()

    counts <- x |>
      dplyr::count(!!explanatory_expr(x), .drop = FALSE) |>
      dplyr::pull()

    attr(res, "se") <-
      sqrt(
        sum(
          abs((props[1] * (1 - props[1])) / counts[1]),
          abs((props[2] * (1 - props[2])) / counts[2])
        )
      )
  }

  res
}

#' @export
calc_impl.diff_in_props <- function(type, x, order, ...) {
  calc_impl.function_of_props(type, x, order, operator = `-`, ...)
}

#' @export
calc_impl.ratio_of_props <- function(type, x, order, ...) {
  calc_impl.function_of_props(type, x, order, operator = `/`, ...)
}

#' @export
calc_impl.odds_ratio <- function(type, x, order, ...) {
  col <- response_expr(x)
  success <- attr(x, "success")

  x |>
    dplyr::group_by(replicate, !!explanatory_expr(x), .drop = FALSE) |>
    dplyr::summarize(prop = mean(!!sym(col) == success, ...)) |>
    dplyr::summarize(
      prop_1 = prop[!!explanatory_expr(x) == order[1]],
      prop_2 = prop[!!explanatory_expr(x) == order[2]],
      stat = (prop_1 / prop_2) / ((1 - prop_1) / (1 - prop_2))
    ) |>
    dplyr::select(stat)
}

#' @export
calc_impl.t <- function(type, x, order, ...) {
  if (theory_type(x) == "Two sample t") {
    x <- reorder_explanatory(x, order)

    df_out <- x |>
      dplyr::summarize(
        stat = stats::t.test(
          !!response_expr(x) ~ !!explanatory_expr(x),
          ...
        )[["statistic"]]
      )
  } else if (theory_type(x) == "One sample t") {
    if (!is_hypothesized(x)) {
      # For bootstrap
      df_out <- x |>
        dplyr::summarize(
          stat = stats::t.test(!!response_expr(x), ...)[["statistic"]]
        )
    } else {
      # For hypothesis testing
      df_out <- x |>
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

#' @export
calc_impl.z <- function(type, x, order, ...) {
  # Two sample proportions
  if (theory_type(x) == "Two sample props z") {
    col <- response_expr(x)
    success <- attr(x, "success")

    x$explan <- factor(
      explanatory_variable(x),
      levels = c(order[1], order[2])
    )

    aggregated <- x |>
      dplyr::group_by(replicate, explan) |>
      dplyr::summarize(
        group_num = dplyr::n(),
        prop = mean(rlang::eval_tidy(col) == rlang::eval_tidy(success)),
        num_suc = sum(rlang::eval_tidy(col) == rlang::eval_tidy(success))
      )

    df_out <- aggregated |>
      dplyr::summarize(
        diff_prop = prop[explan == order[1]] - prop[explan == order[2]],
        total_suc = sum(num_suc),
        n1 = group_num[1],
        n2 = group_num[2],
        p_hat = total_suc / (n1 + n2),
        denom = sqrt(p_hat * (1 - p_hat) / n1 + p_hat * (1 - p_hat) / n2),
        stat = diff_prop / denom
      ) |>
      dplyr::select(stat)

    df_out
  } else if (theory_type(x) == "One sample prop z") {
    # One sample proportion

    # When `hypothesize()` has been called
    success <- attr(x, "success")
    col <- response_expr(x)
    p0 <- unname(attr(x, "params")[1])
    num_rows <- nrow(x) / length(unique(x$replicate))

    df_out <- x |>
      dplyr::summarize(
        stat = (mean(rlang::eval_tidy(col) == rlang::eval_tidy(success), ...) -
          p0) /
          sqrt((p0 * (1 - p0)) / num_rows)
      )

    df_out
  }
}

#' @export
calc_impl.function <- function(type, x, order, ..., call = rlang::caller_env()) {
  rlang::try_fetch(
    {
      if (!identical(dplyr::group_vars(x), "replicate")) {
         x <- dplyr::group_by(x, replicate)
      }
      x_by_replicate <- dplyr::group_split(x)
      res <- purrr::map(x_by_replicate, ~type(.x, order, ...))
    },
    error = function(cnd) {rethrow_stat_cnd(cnd, call = call)},
    warning = function(cnd) {rethrow_stat_cnd(cnd, call = call)}
  )

  if (!rlang::is_scalar_atomic(res[[1]])) {
    cli::cli_abort(
      c(
        "The supplied {.arg stat} function must return a scalar value.",
        "i" = "It returned {.obj_type_friendly {res[[1]]}}."
      ),
      call = call
    )
  }

  tibble::new_tibble(list(stat = unlist(res)))
}

rethrow_stat_cnd <- function(cnd, call = call) {
  cli::cli_abort(
    "The supplied {.arg stat} function encountered an issue.",
    parent = cnd,
    call = call
  )
}
