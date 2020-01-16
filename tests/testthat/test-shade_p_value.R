context("shade_p_value")

library(vdiffr)


# shade_p_value -----------------------------------------------------------
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
    "pval-theor-right", iris_viz_theor + shade_p_value(1, "right")
  )
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

test_that("shade_p_value accepts synonyms for 'direction'", {
  expect_doppelganger(
    "pval-direction-right", iris_viz_sim + shade_p_value(1, "greater")
  )
  expect_doppelganger(
    "pval-direction-left", iris_viz_sim + shade_p_value(1, "less")
  )
  expect_doppelganger(
    "pval-direction-both", iris_viz_sim + shade_p_value(1, "two_sided")
  )
})

test_that("shade_p_value uses extra aesthetic", {
  expect_doppelganger(
    "pval-extra-aes-1",
    iris_viz_sim + shade_p_value(1, "two_sided", alpha = 1)
  )
  expect_doppelganger(
    "pval-extra-aes-2",
    iris_viz_sim + shade_p_value(1, "two_sided", linetype = "dotted")
  )
  expect_doppelganger(
    "pval-extra-aes-3",
    iris_viz_sim + shade_p_value(1, "two_sided", size = 4)
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
  expect_error(iris_viz_sim %>% shade_p_value(1, "right"), 
               "\\`shade_p_value()\\` as a layer")
  expect_error(iris_viz_sim %>% shade_p_value(obs_stat = 1), 
               "\\`shade_p_value()\\` as a layer")
  expect_error(iris_viz_sim %>% shade_p_value(obs_stat = 1, 
                                              direction = "right"), 
               "\\`shade_p_value()\\` as a layer")
})


# norm_direction ----------------------------------------------------------
test_that("norm_direction works", {
  expect_equal(norm_direction("left"), "left")
  expect_equal(norm_direction("less"), "left")
  expect_equal(norm_direction("right"), "right")
  expect_equal(norm_direction("greater"), "right")
  expect_equal(norm_direction("both"), "both")
  expect_equal(norm_direction("two_sided"), "both")
})
