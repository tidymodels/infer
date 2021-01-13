context("shade_confidence_interval")

# shade_confidence_interval -----------------------------------------------
test_that("shade_confidence_interval works", {
  skip_if(getRversion() > "4.0.2")
  
  # Adding `shade_confidence_interval()` to simulation plot
  expect_doppelganger(
    "ci-sim-fill",
    gss_viz_sim + shade_confidence_interval(c(-1, 1))
  )
  expect_doppelganger(
    "ci-sim-nofill",
    gss_viz_sim + shade_confidence_interval(c(-1, 1), fill = NULL)
  )
  
  # Adding `shade_confidence_interval()` to theoretical plot  
  expect_doppelganger(
    "ci-theor-fill",
    gss_viz_theor + shade_confidence_interval(c(-1, 1))
  )
  expect_doppelganger(
    "ci-theor-nofill",
    gss_viz_theor + shade_confidence_interval(c(-1, 1), fill = NULL)
  )
  
  # Adding `shade_confidence_interval()` to "both" plot
  expect_doppelganger(
    "ci-both-fill",
    gss_viz_both + shade_confidence_interval(c(-1, 1))
  )
  expect_doppelganger(
    "ci-both-nofill",
    gss_viz_both + shade_confidence_interval(c(-1, 1), fill = NULL)
  )
})

test_that("shade_confidence_interval accepts `NULL` as `endpoints`",  {
  skip_if(getRversion() > "4.0.2")
  
  expect_doppelganger(
    "ci-null-endpoints",
    gss_viz_sim + shade_confidence_interval(NULL)
  )
})

test_that("shade_confidence_interval uses extra aesthetic", {
  skip_if(getRversion() > "4.0.2")
  
  expect_doppelganger(
    "ci-extra-aes-1",
    gss_viz_sim + shade_confidence_interval(c(-1, 1), alpha = 1)
  )
  expect_doppelganger(
    "ci-extra-aes-2",
    gss_viz_sim + shade_confidence_interval(c(-1, 1), linetype = "dotted")
  )
})

test_that("shade_confidence_interval throws errors and warnings", {
  skip_if(getRversion() > "4.0.2")
  
  expect_warning(gss_viz_sim + shade_confidence_interval(c(1, 2, 3)), "2")
  expect_error(
    gss_viz_sim + shade_confidence_interval(data.frame(x = 1)),
    "1 x 2"
  )
  expect_error(
    gss_viz_sim + shade_confidence_interval(c(-1, 1), color = "x"),
    "color"
  )
  expect_error(
    gss_viz_sim + shade_confidence_interval(c(-1, 1), fill = "x"),
    "color"
  )
  expect_error(
    gss_viz_sim %>% shade_confidence_interval(c(-1, 1)), 
    "\\`shade_confidence_interval\\(\\)\\` as a layer"
  )
  expect_error(
    gss_viz_sim %>% shade_confidence_interval(endpoints = c(-1, 1)), 
    "\\`shade_confidence_interval\\(\\)\\` as a layer"
  )
  expect_error(
    gss_viz_sim %>% shade_ci(c(-1, 1)), 
    "\\`shade_ci\\(\\)\\` as a layer"
  )
  expect_error(
    gss_viz_sim %>% shade_ci(endpoints = c(-1, 1)), 
    "\\`shade_ci\\(\\)\\` as a layer"
  )
})


# shade_ci ----------------------------------------------------------------
# Tested in `shade_confidence_interval()`
