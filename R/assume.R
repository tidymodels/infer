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
#' null distributions can.
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
#' @param df The degrees of freedom parameter(s) for the `distribution`
#'   supplied, as a numeric vector. For `distribution = "F"`, this should have
#'   length two (e.g. `c(10, 3)`). For `distribution = "Chisq"` or 
#'   `distribution = "t"`, this should have length one. For 
#'   `distribution = "z"`, this argument is required.
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
#' @examples   
#' # construct theoretical distributions ---------------------------------
#' 
#' # F distribution
#' # with the `partyid` explanatory variable
#' gss %>% 
#'   specify(age ~ partyid) %>% 
#'   hypothesize(null = "independence") %>%
#'   assume(
#'     distribution = "F", 
#'     c(length(unique(gss$partyid)) - 1, nrow(gss) - 1)
#'   )
#' 
#' # Chi-squared goodness of fit distribution
#' # on the `finrela` variable
#' gss %>%
#'   specify(response = finrela) %>%
#'     hypothesize(null = "point",
#'                 p = c("far below average" = 1/6,
#'                       "below average" = 1/6,
#'                       "average" = 1/6,
#'                       "above average" = 1/6,
#'                       "far above average" = 1/6,
#'                       "DK" = 1/6)) %>%
#'   assume("Chisq", length(unique(gss$finrela)) - 1)
#' 
#' # Chi-squared test of independence
#' # on the `finrela` and `sex` variables
#' gss %>%
#'   specify(formula = finrela ~ sex) %>%
#'   hypothesize(null = "independence") %>%
#'   assume(
#'     distribution = "Chisq", 
#'     df = (length(unique(gss$finrela)) - 1) * 
#'          (length(unique(gss$sex)) - 1)
#'   )
#' 
#' # T distribution
#' gss %>% 
#'   specify(age ~ college) %>%
#'   hypothesize(null = "independence") %>%
#'   assume("t", nrow(gss) - 1)
#' 
#' # Z distribution
#' gss %>%
#'   specify(response = sex, success = "female") %>%
#'   hypothesize(null = "point", p = .5) %>%
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
#'   hypothesize(null = "point", mu = 40) %>%
#'   assume("t", nrow(gss) - 1)
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
#'   hypothesize(null = "independence") %>%
#'   assume(
#'     distribution = "F", 
#'     c(length(unique(gss$partyid)) - 1, nrow(gss) - 1)
#'   )
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
    stop_glue(
      "The `x` argument must be the output of a core infer function, ",
      "likely `specify()` or `hypothesize()`."
    )
  }
  
  # check that `distribution` aligns with what is expected from
  # `x` and that `distribution` and `df` are consistent with each other
  check_distribution(x, distribution, df, ...)
  
  structure(
    glue_null(
      "{distribution_desc(distribution)} distribution{df_desc(df)}.",
    ),
    # store distribution as the suffix to p* in dist function
    distribution = dist_fn(distribution),
    dist_ = distribution,
    # store df for easier passing to p* functions
    df = df,
    # store df in `specify`-esque format for use in `visualize`
    distr_param = if (length(df) > 0) {df[1]} else {NULL},
    distr_param2 = if (length(df) == 2) {df[2]} else {NULL},
    # bring along theory_type x attribute
    theory_type = attr(x, "theory_type"),
    # bring along dots
    dots = list(...),
    # append class
    class = "infer_dist"
  )
}

# check that the distribution is well-specified
check_distribution <- function(x, distribution, df, ...) {
  dist <- tolower(distribution)
  
  if (!dist %in% c("f", "chisq", "t", "z")) {
    stop_glue(
      'The distribution argument must be one of "Chisq", "F", "t", or "z".'
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
      msg_tail <- glue_null(
        "a {get_stat_type_desc(determine_variable_type(x, 'explanatory'))} ",
        "explanatory variable ({explanatory_name(x)})."
      )
    } else {
      msg_tail <- "no explanatory variable."
    }
    
    stop_glue(
      'The supplied distribution "{distribution}" is not well-defined for a ',
      "{get_stat_type_desc(determine_variable_type(x, 'response'))} response ",
      "variable ({response_name(x)}) and ", msg_tail
    )
  }
  
  if (!is.numeric(df) && !is.null(df)) {
    stop_glue(
      "`assume()` expects the `df` argument to be a numeric vector, ",
      "but you supplied a {list(class(df))} object."
    )
  }
  
  if (length(list(...)) != 0) {
    plural <- length(list(...)) != 1
    dots <- list(...)
    
    stop_glue(
      "`assume()` ignores the dots `...` argument, though the ",
      "argument{if (plural) 's' else ''} `{list(dots)}` ",
      "{if (plural) 'were' else 'was'} supplied. Did you forget to ",
      "concatenate the `df` argument with `c()`?"
    )
  }
  
  if (dist_df_length(distribution) != length(df)) {
    plural <- length(df) != 1
    stop_glue(
      '{distribution_desc(distribution)} distribution requires ',
      '{dist_df_length(distribution)} degrees of freedom argument',
      '{if (!plural) "s" else ""}, but {length(df)} ',
      '{if (plural) "were" else "was"} supplied.'
    )
  }
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
      if (plural) {paste0(df, collapse = " and ")} else {df}, 
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