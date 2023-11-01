#' Define a theoretical distribution
#'
#' @description
#'
#' This function allows the user to define a null distribution based on
#' theoretical methods. In many infer pipelines, `assume()` can be
#' used in place of [generate()] and [calculate()] to create a null
#' distribution. Rather than outputting a data frame containing a
#' distribution of test statistics calculated from resamples of the observed
#' data, `assume()` outputs a more abstract type of object just containing
#' the distributional details supplied in the `distribution` and `df` arguments.
#' However, `assume()` output can be passed to [visualize()], [get_p_value()],
#' and [get_confidence_interval()] in the same way that simulation-based
#' distributions can.
#'
#' To define a theoretical null distribution (for use in hypothesis testing),
#' be sure to provide a null hypothesis via [hypothesize()]. To define a
#' theoretical sampling distribution (for use in confidence intervals),
#' provide the output of [specify()]. Sampling distributions (only
#' implemented for `t` and `z`) lie on the scale of the data, and will be
#' recentered and rescaled to match the corresponding `stat` given in
#' [calculate()] to calculate the observed statistic.
#'
#' @param x The output of [specify()] or [hypothesize()], giving the
#'   observed data, variable(s) of interest, and (optionally) null hypothesis.
#' @param distribution The distribution in question, as a string. One of
#'   `"F"`, `"Chisq"`, `"t"`, or `"z"`.
#' @param df Optional. The degrees of freedom parameter(s) for the `distribution`
#'   supplied, as a numeric vector. For `distribution = "F"`, this should have
#'   length two (e.g. `c(10, 3)`). For `distribution = "Chisq"` or
#'   `distribution = "t"`, this should have length one. For
#'   `distribution = "z"`, this argument is not required. The package
#'   will supply a message if the supplied `df` argument is different from
#'   recognized values. See the Details section below for more information.
#' @param ... Currently ignored.
#'
#' @return An infer theoretical distribution that can be passed to helpers
#'   like [visualize()], [get_p_value()], and [get_confidence_interval()].
#'
#' @details
#'
#' Note that the assumption being expressed here, for use in theory-based
#' inference, only extends to _distributional_ assumptions: the null
#' distribution in question and its parameters. Statistical inference with
#' infer, whether carried out via simulation (i.e. based on pipelines
#' using [generate()] and [calculate()]) or theory (i.e. with `assume()`),
#' always involves the condition that observations are independent of
#' each other.
#'
#' `infer` only supports theoretical tests on one or two means via the
#' `t` distribution and one or two proportions via the `z`.
#'
#' For tests comparing two means, if `n1` is the group size for one level of
#' the explanatory variable, and `n2` is that for the other level, `infer`
#' will recognize the following degrees of freedom (`df`) arguments:
#'
#' * `min(n1 - 1, n2 - 1)`
#' * `n1 + n2 - 2`
#' * The `"parameter"` entry of the analogous `stats::t.test()` call
#' * The `"parameter"` entry of the analogous `stats::t.test()` call with `var.equal = TRUE`
#'
#' By default, the package will use the `"parameter"` entry of the analogous
#' `stats::t.test()` call with `var.equal = FALSE` (the default).
#'
#' @examples
#' # construct theoretical distributions ---------------------------------
#'
#' # F distribution
#' # with the `partyid` explanatory variable
#' gss %>%
#'   specify(age ~ partyid) %>%
#'   assume(distribution = "F")
#'
#' # Chi-squared goodness of fit distribution
#' # on the `finrela` variable
#' gss %>%
#'   specify(response = finrela) %>%
#'   hypothesize(null = "point",
#'               p = c("far below average" = 1/6,
#'                     "below average" = 1/6,
#'                     "average" = 1/6,
#'                     "above average" = 1/6,
#'                     "far above average" = 1/6,
#'                     "DK" = 1/6)) %>%
#'   assume("Chisq")
#'
#' # Chi-squared test of independence
#' # on the `finrela` and `sex` variables
#' gss %>%
#'   specify(formula = finrela ~ sex) %>%
#'   assume(distribution = "Chisq")
#'
#' # T distribution
#' gss %>%
#'   specify(age ~ college) %>%
#'   assume("t")
#'
#' # Z distribution
#' gss %>%
#'   specify(response = sex, success = "female") %>%
#'   assume("z")
#'
#' \dontrun{
#' # each of these distributions can be passed to infer helper
#' # functions alongside observed statistics!
#'
#' # for example, a 1-sample t-test -------------------------------------
#'
#' # calculate the observed statistic
#' obs_stat <- gss %>%
#'   specify(response = hours) %>%
#'   hypothesize(null = "point", mu = 40) %>%
#'   calculate(stat = "t")
#'
#' # construct a null distribution
#' null_dist <- gss %>%
#'   specify(response = hours) %>%
#'   assume("t")
#'
#' # juxtapose them visually
#' visualize(null_dist) +
#'   shade_p_value(obs_stat, direction = "both")
#'
#' # calculate a p-value
#' get_p_value(null_dist, obs_stat, direction = "both")
#'
#' # or, an F test ------------------------------------------------------
#'
#' # calculate the observed statistic
#' obs_stat <- gss %>%
#'   specify(age ~ partyid) %>%
#'   hypothesize(null = "independence") %>%
#'   calculate(stat = "F")
#'
#' # construct a null distribution
#' null_dist <- gss %>%
#'   specify(age ~ partyid) %>%
#'   assume(distribution = "F")
#'
#' # juxtapose them visually
#' visualize(null_dist) +
#'   shade_p_value(obs_stat, direction = "both")
#'
#' # calculate a p-value
#' get_p_value(null_dist, obs_stat, direction = "both")
#' }
#'
#' @export
assume <- function(x, distribution, df = NULL, ...) {
  if (!inherits(x, "infer")) {
    cli_abort(
      "The {.arg x} argument must be the output of a core infer function, \\
       likely {.fun specify} or {.fun hypothesize}."
    )
  }

  # check that `distribution` aligns with what is expected from
  # `x` and that `distribution` and `df` are consistent with each other
  df <- check_distribution(x, distribution, df, ...)

  structure(
    glue(
      "{distribution_desc(distribution)} distribution{df_desc(df)}.",
      .null = "NULL"
    ),
    # store distribution as the suffix to p* in dist function
    distribution = dist_fn(distribution),
    dist_ = distribution,
    # store df for easier passing to p* functions
    df = df,
    # store df in `specify`-esque format for use in `visualize`
    distr_param = if (length(df) > 0) {df[1]} else {NULL},
    distr_param2 = if (length(df) == 2) {df[2]} else {NULL},
    # bring along x attributes
    theory_type = attr(x, "theory_type"),
    params = attr(x, "params"),
    hypothesized = attr(x, "hypothesized"),
    # bring along dots
    dots = list(...),
    # append class
    class = "infer_dist"
  )
}

# check that the distribution is well-specified
check_distribution <- function(x, distribution, df, ..., call = caller_env()) {
  dist <- tolower(distribution)

  if (!dist %in% c("f", "chisq", "t", "z")) {
     cli_abort(
      'The distribution argument must be one of "Chisq", "F", "t", or "z".',
      call = call
    )
  }

  if ((dist == "f" && attr(x, "theory_type") != "ANOVA") ||
      (dist == "chisq" && !attr(x, "theory_type") %in% c("Chi-square test of indep",
                                                         "Chi-square Goodness of Fit")) ||
      (dist == "t" && !attr(x, "theory_type") %in% c("One sample t",
                                                     "Two sample t")) ||
      (dist == "z" && !attr(x, "theory_type") %in% c("One sample prop z",
                                                     "Two sample props z"))) {
    if (has_explanatory(x)) {
      msg_tail <- glue(
        "a {get_stat_type_desc(attr(x, 'type_desc_explanatory'))} ",
        "explanatory variable ({explanatory_name(x)}).",
        .null = "NULL"
      )
    } else {
      msg_tail <- "no explanatory variable."
    }

     cli_abort(
      'The supplied distribution {.val {distribution}} is not well-defined for a \\
      {get_stat_type_desc(attr(x, "type_desc_response"))} response \\
      variable ({response_name(x)}) and {msg_tail}', call = call)
  }

  if (!is.numeric(df) && !is.null(df)) {
     cli_abort(
      "{.fun assume} expects the {.arg df} argument to be a numeric vector, \\
       but you supplied a {list(class(df))} object.",
      call = call
     )
  }

  if (length(list(...)) != 0) {
    dots <- list(...)

    cli_abort(c(
      "{.fun assume} ignores the dots `...` argument, though the \\
       {qty(dots)}argument{?s} {.field {names(dots)}} {?was/were} supplied. ",
       i = "Did you forget to concatenate the {.arg df} argument with {.fun c}?"),
      call = call
    )
  }

  if (dist_df_length(distribution) != length(df) && !is.null(df)) {

    cli_abort(
      '{distribution_desc(distribution)} distribution requires \\
       {dist_df_length(distribution)} degrees of freedom argument{?s}, \\
       but {length(df)} {?was/were} supplied.',
      call = call
    )
  }

  df <- determine_df(x, dist, df)

  return(df)
}

# convert the distribution argument to its r distribution function suffix
dist_fn <- function(distribution) {
  switch(
    tolower(distribution),
    `f` = "f",
    `chisq` = "chisq",
    `t` = "t",
    `z` = "norm"
  )
}

# return expected degrees of freedom length
dist_df_length <- function(distribution) {
  switch(
    tolower(distribution),
    `f` = 2,
    `chisq` = , `t` = 1,
    `z` = 0
  )
}

# describe the distribution
distribution_desc <- function(distribution) {
  switch(
    tolower(distribution),
    `f` = "An F",
    `chisq` = "A Chi-squared",
    `t` = "A T",
    `z` = "A Z"
  )
}

# describe the degrees of freedom
df_desc <- function(df) {
  if (is.null(df)) {
    ""
  } else {
    plural <- length(df) != 1

    paste0(
      ' with ',
      if (plural) {paste0(round(df), collapse = " and ")} else {round(df)},
      ' degree',
      if (!plural && df == 1) {''} else {'s'},
      ' of freedom')
  }
}

# process df for passing to p* functions
process_df <- function(df) {
  switch(as.character(length(df)),
    "0" = list(),
    "1" = list(df = df),
    "2" = list(df1 = df[1], df2 = df[2])
  )
}

# generate an automatic "df" value using logic from
# hypothesize and, if it doesn't match the
# supplied one, raise a message
determine_df <- function(x, dist, df) {

  if (!is.null(df) && !all(round(df) %in% round(acceptable_dfs(x)))) {
    cli_inform(
      "Message: The supplied {.arg df} argument does not match its \\
       expected value. If this is unexpected, ensure that your calculation \\
       for {.arg df} is correct (see {.help [{.fun assume}](infer::assume)} for \\
       recognized values) or supply {.code df = NULL} to {.fun assume}."
    )

    return(df)
  }

  if (is.null(df)) {
    df <- acceptable_dfs(x)
  }

  if (attr(x, "theory_type") == "Two sample t") {
    df <- df[1]
  }

  df
}

# return a vector of dfs recognized by `assume`
acceptable_dfs <- function(x) {
  if (attr(x, "theory_type") == "Two sample t") {
    c(
      # t.test param with var.equal = FALSE
      unname(
        unlist(
          attr(x, "distr_param") <-
            stats::t.test(response_variable(x) ~
                          explanatory_variable(x))[["parameter"]]
        )
      ),
      # t.test param with var.equal = TRUE
      unname(
        unlist(
          attr(x, "distr_param") <-
            stats::t.test(response_variable(x) ~
                          explanatory_variable(x),
                          var.equal = TRUE)[["parameter"]]
        )
      ),
      # min(n1 - 1, n2 - 1)
      x %>%
        dplyr::count(!!explanatory_expr(x)) %>%
        dplyr::pull(n) %>%
        min() %>%
        `-`(1),
      # n1 + n2 - 2
      x %>%
        dplyr::count(!!explanatory_expr(x)) %>%
        dplyr::pull(n) %>%
        sum() %>%
        `-`(2)
    )
  } else {
    c(
      unname(unlist(attr(x, "distr_param"))),
      unname(unlist(attr(x, "distr_param2")))
    )
  }
}
