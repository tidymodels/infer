context("assume")

test_that("distribution description works as expected", {
  # extract the "first element" to convert to character
  assume_ <- function(...) {
    assume(...)[1]
  }
  
  expect_equal(
    gss %>% 
      specify(age ~ partyid) %>%
      hypothesize(null = "independence") %>%
      assume_(
       distribution = "F", 
       df = c(length(unique(gss$partyid)) - 1, nrow(gss) - 4)
      ), 
    "An F distribution with 3 and 496 degrees of freedom."
  )
  
  expect_equal(
    gss %>% 
      specify(age ~ partyid) %>%
      hypothesize(null = "independence") %>%
      assume_(
        distribution = "F", 
        df = c(length(unique(gss$partyid)) - 1, nrow(gss) - 4)
      ), 
    gss %>% 
      specify(age ~ partyid) %>%
      hypothesize(null = "independence") %>%
      assume_(distribution = "F")
  )
  
  expect_equal(
    gss %>%
      specify(response = finrela) %>%
      hypothesize(null = "point",
                  p = c("far below average" = 1/6,
                        "below average" = 1/6,
                        "average" = 1/6,
                        "above average" = 1/6,
                        "far above average" = 1/6,
                        "DK" = 1/6)) %>%
      assume_("Chisq", length(unique(gss$finrela)) - 1), 
    "A Chi-squared distribution with 5 degrees of freedom."
  )
  
  expect_equal(
    gss %>%
      specify(response = finrela) %>%
      hypothesize(null = "point",
                  p = c("far below average" = 1/6,
                        "below average" = 1/6,
                        "average" = 1/6,
                        "above average" = 1/6,
                        "far above average" = 1/6,
                        "DK" = 1/6)) %>%
      assume_("Chisq"), 
    "A Chi-squared distribution with 5 degrees of freedom."
  )
  
  expect_equal(
    gss %>%
      specify(formula = finrela ~ sex) %>%
      hypothesize(null = "independence") %>%
      assume_(
        distribution = "Chisq", 
        df = (length(unique(gss$finrela)) - 1) * 
          (length(unique(gss$sex)) - 1)
      ), 
    "A Chi-squared distribution with 5 degrees of freedom."
  )
  
  expect_equal(
    gss %>%
      specify(formula = finrela ~ sex) %>%
      hypothesize(null = "independence") %>%
      assume_(distribution = "Chisq"), 
    "A Chi-squared distribution with 5 degrees of freedom."
  )
  
  expect_equal(
    gss %>% 
      specify(age ~ college) %>%
      hypothesize(null = "independence") %>%
      assume_("t"), 
    "A T distribution with 423 degrees of freedom."
  )
  
  expect_equal(
    gss %>%
      specify(response = sex, success = "female") %>%
      hypothesize(null = "point", p = .5) %>%
      assume_("z"), 
    "A Z distribution."
  )
})

test_that("assume errors with bad arguments", {
  # supply a bad distribution
  expect_error(
    gss %>% 
      specify(age ~ college) %>%
      hypothesize(null = "independence") %>%
      assume("boop", nrow(gss) - 1), 
    'The distribution argument must be one of "Chisq", "F", "t", or "z".'
  )
  
  # bad number of df arguments
  expect_error(
    gss %>% 
      specify(age ~ college) %>%
      hypothesize(null = "independence") %>%
      assume("t", c(nrow(gss) - 1, 2)), 
    'A T distribution requires 1 degrees of freedom argument, but 2 were supplied.'
  )
  
  expect_error(
    gss %>% 
      specify(age ~ partyid) %>% 
      hypothesize(null = "independence") %>%
      assume("F", nrow(gss) - 1), 
    'An F distribution requires 2 degrees of freedom arguments, but 1 was supplied.'
  )
  
  # bad df argument type
  expect_error(
    gss %>% 
      specify(age ~ partyid) %>% 
      hypothesize(null = "independence") %>%
      assume("F", "boop"), 
    'to be a numeric vector, but you supplied a character object.'
  )
  
  # df argument possibly passed to dots
  expect_error(
    gss %>% 
      specify(age ~ partyid) %>% 
      hypothesize(null = "independence") %>%
      assume("F", nrow(gss) - 1, 1), 
    'though the argument `list\\(1\\)` was supplied'
  )
  
  expect_error(
    gss %>% 
      specify(age ~ partyid) %>% 
      hypothesize(null = "independence") %>%
      assume("F", nrow(gss) - 1, 1, 2), 
    'arguments `list\\(1, 2\\)` were supplied'
  )
  
  # supply `distribution`s that don't align with the supplied variables
  expect_error(
    gss %>% 
      specify(age ~ finrela) %>% 
      hypothesize(null = "independence") %>%
      assume("t", nrow(gss) - 1),
    'supplied distribution "t" is not well-defined.*onse variable \\(age\\)'
  )
  
  expect_error(
    gss %>% 
      specify(age ~ finrela) %>% 
      hypothesize(null = "independence") %>%
      assume("z", nrow(gss) - 1),
    'supplied distribution "z" is not well-defined.*onse variable \\(age\\)'
  )
  
  expect_error(
    gss %>% 
      specify(age ~ NULL) %>% 
      hypothesize(null = "point", mu = 40) %>%
      assume("z", nrow(gss) - 1),
    'supplied distribution "z" is not well-defined.*onse variable \\(age\\)'
  )
  
  # supply bad `x` arguments
  expect_error(
    gss %>% 
      assume("z", nrow(gss) - 1),
    '`x` argument must be the output of a core infer function'
  )
  
  expect_error(
    "boop" %>%
      assume("z", nrow(gss) - 1),
    '`x` argument must be the output of a core infer function'
  )
})

test_that("assume() handles automatic df gracefully", {
  expect_equal(
    expect_silent(
      gss %>%
        specify(response = hours) %>%
        hypothesize(null = "point", mu = 40) %>%
        assume("t")
    ),
    expect_silent(
      gss %>%
        specify(response = hours) %>%
        hypothesize(null = "point", mu = 40) %>%
        assume("t")
    )
  )
  
  expect_message(
    gss %>%
      specify(response = hours) %>%
      hypothesize(null = "point", mu = 40) %>%
      assume("t", nrow(gss) - 2),
    "does not match its expected value..*calculation for `df`"
  )
})

test_that("assume() brings along supplied arguments", {
  t_dist <-  gss %>% 
    specify(age ~ college) %>%
    hypothesize(null = "independence") %>%
    assume("t")
  
  expect_equal(
    round(attr(t_dist, "df")),
    423
  )
  
  expect_equal(
    attr(t_dist, "distribution"),
    "t"
  )
  
  expect_equal(
    attr(t_dist, "theory_type"),
    "Two sample t"
  )
  
  expect_equal(
    attr(t_dist, "df"),
    attr(t_dist, "distr_param")
  )
  
  f_dist <- gss %>% 
    specify(age ~ partyid) %>% 
    hypothesize(null = "independence") %>%
    assume(distribution = "F")
  
  expect_equal(
    attr(f_dist, "df"),
    c(attr(f_dist, "distr_param"), attr(f_dist, "distr_param2"))
  )
})

test_that("process_df works", {
  expect_equal(
    process_df(1),
    list(df = 1)
  )
  
  expect_equal(
    process_df(c(1, 2)),
    list(df1 = 1, df2 = 2)
  )
  
  expect_equal(
    process_df(NULL),
    list()
  )
})
