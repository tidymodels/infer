context("shade_p_value")

test_that("shade_p_value works", {
  # Adding `shade_p_value()` to simulation plot
  expect_doppelganger(
    "pval-sim-right", iris_viz_sim + shade_p_value(1, "right")
  )
  expect_doppelganger("pval-sim-left", iris_viz_sim + shade_p_value(1, "left"))
  expect_doppelganger("pval-sim-both", iris_viz_sim + shade_p_value(1, "both"))
  expect_doppelganger("pval-sim-null", iris_viz_sim + shade_p_value(1, NULL))
  expect_doppelganger(
    "pval-sim-corrupt",
    expect_warning(iris_viz_sim + shade_p_value(1, "aaa"), "direction")
  )
  
  # Adding `shade_p_value()` to theoretical plot
  expect_doppelganger(
    "pval-theor-right", iris_viz_theor + shade_p_value(1, "right"))
  expect_doppelganger(
    "pval-theor-left", iris_viz_theor + shade_p_value(1, "left")
  )
  expect_doppelganger(
    "pval-theor-both", iris_viz_theor + shade_p_value(1, "both")
  )
  expect_doppelganger(
    "pval-theor-null", iris_viz_theor + shade_p_value(1, NULL)
  )
  expect_doppelganger(
    "pval-theor-corrupt",
    expect_warning(iris_viz_theor + shade_p_value(1, "aaa"), "direction")
  )
  
  # Adding `shade_p_value()` to "both" plot
  expect_doppelganger(
    "pval-both-right", iris_viz_both + shade_p_value(1, "right")
  )
  expect_doppelganger(
    "pval-both-left", iris_viz_both + shade_p_value(1, "left")
  )
  expect_doppelganger(
    "pval-both-both", iris_viz_both + shade_p_value(1, "both")
  )
  expect_doppelganger(
    "pval-both-null", iris_viz_both + shade_p_value(1, NULL)
  )
  expect_doppelganger(
    "pval-both-corrupt",
    expect_warning(iris_viz_both + shade_p_value(1, "aaa"), "direction")
  )
})

test_that("shade_p_value accepts `NULL` as `obs_stat`",  {
  expect_doppelganger(
    "pval-null-obs_stat", iris_viz_sim + shade_p_value(NULL, "left")
  )
})

test_that("shade_p_value throws errors", {
  expect_error(iris_viz_sim + shade_p_value("a", "right"), "numeric")
  expect_error(iris_viz_sim + shade_p_value(1, 1), "character")
  expect_error(iris_viz_sim + shade_p_value(1, "right", color = "x"), "color")
  expect_error(iris_viz_sim + shade_p_value(1, "right", fill = "x"), "color")
})
