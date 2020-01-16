context("shade_confidence_interval")

library(vdiffr)


# shade_confidence_interval -----------------------------------------------
test_that("shade_confidence_interval works", {
  # Adding `shade_confidence_interval()` to simulation plot
  expect_doppelganger(
    "ci-sim-fill",
    iris_viz_sim + shade_confidence_interval(c(-1, 1))
  )
  expect_doppelganger(
    "ci-sim-nofill",
    iris_viz_sim + shade_confidence_interval(c(-1, 1), fill = NULL)
  )
  
  # Adding `shade_confidence_interval()` to theoretical plot  
  expect_doppelganger(
    "ci-theor-fill",
    iris_viz_theor + shade_confidence_interval(c(-1, 1))
  )
  expect_doppelganger(
    "ci-theor-nofill",
    iris_viz_theor + shade_confidence_interval(c(-1, 1), fill = NULL)
  )
  
  # Adding `shade_confidence_interval()` to "both" plot
  expect_doppelganger(
    "ci-both-fill",
    iris_viz_both + shade_confidence_interval(c(-1, 1))
  )
  expect_doppelganger(
    "ci-both-nofill",
    iris_viz_both + shade_confidence_interval(c(-1, 1), fill = NULL)
  )
})

test_that("shade_confidence_interval accepts `NULL` as `endpoints`",  {
  expect_doppelganger(
    "ci-null-endpoints",
    iris_viz_sim + shade_confidence_interval(NULL)
  )
})

test_that("shade_confidence_interval uses extra aesthetic", {
  expect_doppelganger(
    "ci-extra-aes-1",
    iris_viz_sim + shade_confidence_interval(c(-1, 1), alpha = 1)
  )
  expect_doppelganger(
    "ci-extra-aes-2",
    iris_viz_sim + shade_confidence_interval(c(-1, 1), linetype = "dotted")
  )
})

test_that("shade_confidence_interval throws errors and warnings", {
  expect_warning(iris_viz_sim + shade_confidence_interval(c(1, 2, 3)), "2")
  expect_error(
    iris_viz_sim + shade_confidence_interval(data.frame(x = 1)),
    "1 x 2"
  )
  expect_error(
    iris_viz_sim + shade_confidence_interval(c(-1, 1), color = "x"),
    "color"
  )
  expect_error(
    iris_viz_sim + shade_confidence_interval(c(-1, 1), fill = "x"),
    "color"
  )
  expect_error(
    iris_viz_sim %>% shade_confidence_interval(c(-1, 1)), 
    "adding the result"
  )
  expect_error(
    iris_viz_sim %>% shade_confidence_interval(endpoints = c(-1, 1)), 
    "adding the result"
  )
})


# shade_ci ----------------------------------------------------------------
# Tested in `shade_confidence_interval()`
