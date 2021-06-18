#' Create a theoretical distribution
#' 
#' @description 
#' 
#' Fill in once interface solidifies.
#' 
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
#' @examples   
#' # construct theoretical distributions ---------------------------------
#' 
#' # F distribution
#' # with the `partyid` explanatory variable
#' assume(
#'   distribution = "F", 
#'   c(length(unique(gss$partyid)) - 1, nrow(gss) - 1)
#' )
#' 
#' # Chi-squared goodness of fit distribution
#' # on the `finrela` variable
#' assume("Chisq", length(unique(gss$finrela)) - 1)
#' 
#' # Chi-squared test of independence
#' # on the `finrela` and `sex` variables
#' assume(
#'   distribution = "Chisq", 
#'   df = (length(unique(gss$finrela)) - 1) * 
#'        (length(unique(gss$sex)) - 1)
#' )
#' 
#' # T distribution
#' assume("t", nrow(gss) - 1)
#' 
#' # Z distribution
#' assume("z")
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
#' null_dist <- assume("t", nrow(gss) - 1)
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
#'   calculate(stat = "F")
#' 
#' # construct a null distribution
#' null_dist <- 
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
assume <- function(distribution, df = NULL, ...) {
  check_distribution(distribution, df, ...)
  
  structure(
    glue_null(
      "{distribution_desc(distribution)} distribution{df_desc(df)}.",
    ),
    distribution = dist_fn(distribution), 
    df = df,
    dots = list(...),
    class = "infer_dist"
  )
}

# check that the distribution is well-specified
check_distribution <- function(distribution, df, ...) {
  dist <- tolower(distribution)
  
  if (!dist %in% c("f", "chisq", "t", "z")) {
    stop_glue(
      'The distribution argument must be one of "Chisq", "F", "t", or "z".'
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
