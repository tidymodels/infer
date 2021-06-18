context("assume")

test_that("distribution description works as expected", {
  # extract the "first element" to convert to character
  assume_ <- function(...) {
    assume(...)[1]
  }
  
  expect_equal(
    assume_(
      distribution = "F", 
      c(length(unique(gss$partyid)) - 1, nrow(gss) - 1)
    ), 
    "An F distribution with 3 and 499 degrees of freedom."
  )
  
  expect_equal(
    assume_("Chisq", length(unique(gss$finrela)) - 1), 
    "A Chi-squared distribution with 5 degrees of freedom."
  )
  
  expect_equal(
    assume_(
      distribution = "Chisq", 
      df = (length(unique(gss$finrela)) - 1) * 
        (length(unique(gss$sex)) - 1)
    ), 
    "A Chi-squared distribution with 5 degrees of freedom."
  )
  
  expect_equal(
    assume_("t", nrow(gss) - 1), 
    "A T distribution with 499 degrees of freedom."
  )
  
  expect_equal(
    assume_("z"), 
    "A Z distribution."
  )
})

test_that("assume errors with bad arguments", {
  # supply a bad distribution
  expect_error(
    assume("boop", nrow(gss) - 1), 
    'The distribution argument must be one of "Chisq", "F", "t", or "z".'
  )
  
  # bad number of df arguments
  expect_error(
    assume("t", c(nrow(gss) - 1, 2)), 
    'A T distribution requires 1 degrees of freedom argument, but 2 were supplied.'
  )
  
  expect_error(
    assume("F", nrow(gss) - 1), 
    'An F distribution requires 2 degrees of freedom arguments, but 1 was supplied.'
  )
  
  # bad df argument type
  expect_error(
    assume("F", "boop"), 
    'to be a numeric vector, but you supplied a character object.'
  )
  
  # df argument possibly passed to dots
  expect_error(
    assume("F", nrow(gss) - 1, 1), 
    'though the argument `list\\(1\\)` was supplied'
  )
  
  expect_error(
    assume("F", nrow(gss) - 1, 1, 2), 
    'arguments `list\\(1, 2\\)` were supplied'
  )
})

test_that("assume() brings along supplied arguments", {
  t_dist <- assume("t", nrow(gss) - 1)
  
  expect_equal(
    attr(t_dist, "df"),
    499
  )
  
  expect_equal(
    attr(t_dist, "distribution"),
    "t"
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
