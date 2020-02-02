#' Calculate summary statistics
#'
#' @description
#' \lifecycle{maturing}
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
#'   `"F"`, `"t"`, `"z"`, `"slope"`, and `"correlation"`.
#' @param order A string vector of specifying the order in which the levels of
#'   the explanatory variable should be ordered for subtraction, where `order =
#'   c("first", "second")` means `("first" - "second")` Needed for inference on
#'   difference in means, medians, or proportions and t and z statistics.
#' @param ... To pass options like `na.rm = TRUE` into functions like
#'   [mean()][base::mean()], [sd()][stats::sd()], etc.
#'
#' @return A tibble containing a `stat` column of calculated statistics.
#'
#' @examples
#'
#' # calculate a null distribution of hours worked per week under
#' # the null hypothesis that the mean is 40
#' gss %>%
#'  specify(response = hours) %>%
#'  hypothesize(null = "point", mu = 40) %>%
#'  generate(reps = 1000, type = "bootstrap") %>%
#'  calculate(stat = "mean")
#'
#' # calculate a null distribution assuming independence between age
#' # of respondent and whether they have a college degree
#' gss %>%
#'  specify(age ~ college) %>%
#'  hypothesize(null = "independence") %>%
#'  generate(reps = 1000, type = "permute") %>%
#'  calculate("diff in means", order = c("degree", "no degree"))
#'
#' # More in-depth explanation of how to use the infer package
#' vignette("infer")
#'
#' @importFrom dplyr group_by summarize n
#' @importFrom rlang !! sym quo enquo eval_tidy
#' @export
calculate <- function(x,
                      stat = c(
                        "mean", "median", "sum", "sd", "prop", "count",
                        "diff in means", "diff in medians", "diff in props",
                        "Chisq", "F", "slope", "correlation", "t", "z"
                      ),
                      order = NULL,
                      ...) {
  check_type(x, tibble::is_tibble)
  check_type(stat, rlang::is_string)
  check_for_numeric_stat(x, stat)
  check_for_factor_stat(x, stat, explanatory_variable(x))
  check_args_and_attr(x, explanatory_variable(x), response_variable(x), stat)
  check_point_params(x, stat)

  if (!has_response(x)) {
    stop_glue(
      "The response variable is not set. Make sure to `specify()` it first."
    )
  }

  if (is_nuat(x, "generate") || !attr(x, "generate")) {
    if (!is_hypothesized(x)) {
      x$replicate <- 1L
    } else if (
      stat %in% c(
        "mean", "median", "sum", "sd", "prop", "count", "diff in means",
        "diff in medians", "diff in props", "slope", "correlation"
      )
    ) {
      stop_glue(
        "Theoretical distributions do not exist (or have not been ",
        "implemented) for `stat` = \"{stat}\". Are you missing ",
        "a `generate()` step?"
      )
      } else if (!(stat %in% c("Chisq", "prop", "count")) &
                 !(stat == "t" & (attr(x, "theory_type") == "One sample t"))) {
      # From `hypothesize()` to `calculate()`
      # Catch-all if generate was not called
#      warning_glue("You unexpectantly went from `hypothesize()` to ",
#                   "`calculate()` skipping over `generate()`. Your current ",
#                   "data frame is returned.")
      return(x)
    }
  }
  
  if (
    (stat %in% c("diff in means", "diff in medians", "diff in props")) ||
    (
      !is_nuat(x, "theory_type") &&
      (attr(x, "theory_type") %in% c("Two sample props z", "Two sample t"))
    )
  ) {
    order <- check_order(x, explanatory_variable(x), order)
  }

  if (!(
    (stat %in% c("diff in means", "diff in medians", "diff in props")) ||
    (
      !is_nuat(x, "theory_type") &&
      attr(x, "theory_type") %in% c("Two sample props z", "Two sample t")
    )
  )) {
    if (!is.null(order)) {
      warning_glue(
        "Statistic is not based on a difference; the `order` argument ",
        "is ignored. Check `?calculate` for details."
      )
    }
  }

  # Use S3 method to match correct calculation
  result <- calc_impl(
    structure(stat, class = gsub(" ", "_", stat)), x, order, ...
  )

  if ("NULL" %in% class(result)) {
    stop_glue(
      "Your choice of `stat` is invalid for the types of variables `specify`ed."
    )
  }
#   else {
#     result <- append_infer_class(result)
#   }

  result <- copy_attrs(to = result, from = x)
  attr(result, "stat") <- stat

  # For returning a 1x1 observed statistic value
  if (nrow(result) == 1) {
    result <- select(result, stat)
  }

  result
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
    
    ## No longer needed with implementation of `check_point_params()`
    # if (!is.factor(x[[col]])) {
    #   stop_glue(
    #     "Calculating a {stat} here is not appropriate since the `{col}` ",
    #     "variable is not a factor."
    #   )
    # }
    
    if (is_nuat(x, "success")) {
      stop_glue(
        'To calculate a {output_name}, the `"success"` argument must be ',
        'provided in `specify()`.'
      )
    }
    
    success <- attr(x, "success")
    x %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = f(!!sym(col), success))
  }
}

calc_impl.prop <- calc_impl_success_f(
  f = function(response, success, ...) {mean(response == success, ...)},
  output_name = "proportion"
)

calc_impl.count <- calc_impl_success_f(
  f = function(response, success, ...) {sum(response == success, ...)},
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
      dplyr::group_by(replicate, !!attr(x, "explanatory")) %>%
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
  ## The following could stand to be cleaned up

  if (is_nuat(x, "explanatory")) {
    # Chi-Square Goodness of Fit
    if (!is_nuat(x, "params")) {
      # When `hypothesize()` has been called
      p_levels <- get_par_levels(x)
      x %>%
        dplyr::summarize(
          stat = suppressWarnings(stats::chisq.test(
            # Ensure correct ordering of parameters
            table(!!(attr(x, "response")))[p_levels],
            p = attr(x, "params")
          )$stat
        ))
    } else {
      # Straight from `specify()`
      stop_glue(
        "In order to calculate a Chi-Square Goodness of Fit statistic, ",
        "hypothesized values must be given for the `p` parameter in the ",
        "`hypothesize()` function prior to using `calculate()`"
      )
    }
  } else {
    # This is not matching with chisq.test
    # obs_tab <- x %>%
    #   dplyr::filter(replicate == 1) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::select(!!attr(x, "response"), !!(attr(x, "explanatory"))) %>%
    #   table()
    # expected <- outer(rowSums(obs_tab), colSums(obs_tab)) / n
    # df_out <- x %>%
    #   dplyr::summarize(
    #     stat = sum(
    #       (table(!!(attr(x, "response")), !!(attr(x, "explanatory"))) -
    #          expected)^2 / expected,
    #       ...)
    #   )

    # Chi-Square Test of Independence
    result <- x %>%
      dplyr::do(
        broom::tidy(
          suppressWarnings(stats::chisq.test(
            table(
              .[[as.character(attr(x, "response"))]],
              .[[as.character(attr(x, "explanatory"))]]
            )
          ))
        )
      ) %>%
      dplyr::ungroup()

    if (!is_nuat(x, "generate")) {
      result <- result %>% dplyr::select(replicate, stat = statistic)
    } else {
      result <- result %>% dplyr::select(stat = statistic)
    }

    copy_attrs(
      to = result, from = x,
      attrs = c(
        "response", "success", "explanatory", "response_type",
        "explanatory_type", "distr_param", "distr_param2", "theory_type"
      )
    )
  }
}

calc_impl.diff_in_props <- function(type, x, order, ...) {
  col <- attr(x, "response")
  success <- attr(x, "success")

  x %>%
    dplyr::group_by(replicate, !!attr(x, "explanatory")) %>%
    dplyr::summarize(prop = mean(!!sym(col) == success, ...)) %>%
    dplyr::summarize(
      stat = prop[!!attr(x, "explanatory") == order[1]] -
        prop[!!attr(x, "explanatory") == order[2]]
    )
}

calc_impl.t <- function(type, x, order, ...) {
  # Two sample means

  if (attr(x, "theory_type") == "Two sample t") {
    # Re-order levels
    x <- reorder_explanatory(x, order)

    df_out <- x %>%
      dplyr::summarize(
        stat = stats::t.test(
          !!attr(x, "response") ~ !!attr(x, "explanatory"), ...
        )[["statistic"]]
      )
  }

  # Standardized slope and standardized correlation are commented out
  # since there currently is no way to specify which one and
  # the standardization formulas are different.
  # # Standardized slope
  # else if (
  #   (attr(x, "theory_type") == "Slope/correlation with t") &&
  #   (stat == "slope")
  # ) {
  #   explan_string <- as.character(attr(x, "explanatory"))
  # 
  #   x %>%
  #     dplyr::summarize(
  #       stat = summary(stats::lm(
  #         !!attr(x, "response") ~ !!attr(x, "explanatory")
  #       ))[["coefficients"]][explan_string, "t value"]
  #     )
  # }
  # 
  # # Standardized correlation
  # else if (
  #   (attr(x, "theory_type") == "Slope/correlation with t") &&
  #   (stat == "correlation")
  # ) {
  #   x %>%
  #     dplyr::summarize(
  #       corr = cor(!!attr(x, "explanatory"), !!attr(x, "response"))
  #     ) %>%
  #     dplyr::mutate(stat = corr * (sqrt(nrow(x) - 2)) / sqrt(1 - corr ^ 2))
  # }

  # One sample mean
  else if (attr(x, "theory_type") == "One sample t") {
    # For bootstrap
    if (!is_hypothesized(x)) {
      
      if (is.null(list(...)$mu)) {
        message_glue(
          "No `mu` argument was hypothesized, so the t-test will ",
          "assume a null hypothesis `mu = 0`."
        )
      }
      
      x %>%
        dplyr::summarize(
          stat = stats::t.test(!!attr(x, "response"), ...)[["statistic"]]
        )
      
    } else {
      # For hypothesis testing
      x %>%
        dplyr::summarize(
          stat = stats::t.test(
            !!attr(x, "response"), mu = attr(x, "params"), ...
          )[["statistic"]]
        )
    }
  }
}

calc_impl.z <- function(type, x, order, ...) {
  # Two sample proportions
  if (attr(x, "theory_type") == "Two sample props z") {
    col <- attr(x, "response")
    success <- attr(x, "success")

    x$explan <- factor(
      explanatory_variable(x), levels = c(order[1], order[2])
    )

    aggregated <- x %>%
      dplyr::group_by(replicate, explan) %>%
      dplyr::summarize(
        group_num = n(),
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
      dplyr::select(-total_suc, -n1, -n2)

    df_out
  } else if (attr(x, "theory_type") == "One sample prop z") {
    # One sample proportion
    
    # When `hypothesize()` has been called
    success <- attr(x, "success")

    p0 <- attr(x, "params")[1]
    num_rows <- nrow(x) / length(unique(x$replicate))

    col <- attr(x, "response")
#     if (is.null(success)) {
#       success <- quo(get_par_levels(x)[1])
#     }
#     Error given instead

    df_out <- x %>%
      dplyr::summarize(
        stat = (
          mean(rlang::eval_tidy(col) == rlang::eval_tidy(success), ...) - p0
        ) / sqrt((p0 * (1 - p0)) / num_rows)
      )

    df_out

    # Straight from `specify()` doesn't make sense
    # since standardizing requires a hypothesized value
  }
}
