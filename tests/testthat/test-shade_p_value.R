context("shade_p_value")

library(vdiffr)


# shade_p_value -----------------------------------------------------------
test_that("shade_p_value works", {
  skip_if(getRversion() > "4.0.2")
  
  # Adding `shade_p_value()` to simulation plot
  expect_doppelganger(
    "pval-sim-right", gss_viz_sim + shade_p_value(1, "right")
  )
  expect_doppelganger("pval-sim-left", gss_viz_sim + shade_p_value(1, "left"))
  expect_doppelganger("pval-sim-both", gss_viz_sim + shade_p_value(1, "both"))
  expect_doppelganger("pval-sim-null", gss_viz_sim + shade_p_value(1, NULL))
  expect_doppelganger(
    "pval-sim-corrupt",
    expect_warning(gss_viz_sim + shade_p_value(1, "aaa"), "direction")
  )
  
  # Adding `shade_p_value()` to theoretical plot
  expect_doppelganger(
    "pval-theor-right", gss_viz_theor + shade_p_value(1, "right")
  )
  expect_doppelganger(
    "pval-theor-left", gss_viz_theor + shade_p_value(1, "left")
  )
  expect_doppelganger(
    "pval-theor-both", gss_viz_theor + shade_p_value(1, "both")
  )
  expect_doppelganger(
    "pval-theor-null", gss_viz_theor + shade_p_value(1, NULL)
  )
  expect_doppelganger(
    "pval-theor-corrupt",
    expect_warning(gss_viz_theor + shade_p_value(1, "aaa"), "direction")
  )
  
  # Adding `shade_p_value()` to "both" plot
  expect_doppelganger(
    "pval-both-right", gss_viz_both + shade_p_value(1, "right")
  )
  expect_doppelganger(
    "pval-both-left", gss_viz_both + shade_p_value(1, "left")
  )
  expect_doppelganger(
    "pval-both-both", gss_viz_both + shade_p_value(1, "both")
  )
  expect_doppelganger(
    "pval-both-null", gss_viz_both + shade_p_value(1, NULL)
  )
  expect_doppelganger(
    "pval-both-corrupt",
    expect_warning(gss_viz_both + shade_p_value(1, "aaa"), "direction")
  )
})

test_that("shade_p_value accepts synonyms for 'direction'", {
  skip_if(getRversion() > "4.0.2")
  
  expect_doppelganger(
    "pval-direction-right", gss_viz_sim + shade_p_value(1, "greater")
  )
  expect_doppelganger(
    "pval-direction-left", gss_viz_sim + shade_p_value(1, "less")
  )
  # This currently results into the following {vdiffr} warning:
  # "Duplicated expectations: pval-direction-both, pval-direction-both"
  # However, having same figure here as expectation is exactly the goal of tests
  expect_doppelganger(
    "pval-direction-both", gss_viz_sim + shade_p_value(1, "two_sided")
  )
  expect_doppelganger(
    "pval-direction-both", gss_viz_sim + shade_p_value(1, "two-sided")
  )
  expect_doppelganger(
    "pval-direction-both", gss_viz_sim + shade_p_value(1, "two sided")
  )
})

test_that("shade_p_value uses extra aesthetic", {
  skip_if(getRversion() > "4.0.2")
  
  expect_doppelganger(
    "pval-extra-aes-1",
    gss_viz_sim + shade_p_value(1, "two_sided", alpha = 1)
  )
  expect_doppelganger(
    "pval-extra-aes-2",
    gss_viz_sim + shade_p_value(1, "two_sided", linetype = "dotted")
  )
  expect_doppelganger(
    "pval-extra-aes-3",
    gss_viz_sim + shade_p_value(1, "two_sided", size = 4)
  )
})

test_that("shade_p_value accepts `NULL` as `obs_stat`",  {
  skip_if(getRversion() > "4.0.2")
  
  expect_doppelganger(
    "pval-null-obs_stat", gss_viz_sim + shade_p_value(NULL, "left")
  )
})

test_that("shade_p_value throws errors", {
  skip_if(getRversion() > "4.0.2")
  
  expect_error(gss_viz_sim + shade_p_value("a", "right"), "numeric")
  expect_error(gss_viz_sim + shade_p_value(1, 1), "character")
  expect_error(gss_viz_sim + shade_p_value(1, "right", color = "x"), "color")
  expect_error(gss_viz_sim + shade_p_value(1, "right", fill = "x"), "color")
  expect_error(gss_viz_sim %>% shade_p_value(1, "right"), 
               "\\`shade_p_value\\(\\)\\` as a layer")
  expect_error(gss_viz_sim %>% shade_p_value(obs_stat = 1), 
               "\\`shade_p_value\\(\\)\\` as a layer")
  expect_error(gss_viz_sim %>% shade_p_value(obs_stat = 1, 
                                              direction = "right"), 
               "\\`shade_p_value\\(\\)\\` as a layer")
  expect_error(gss_viz_sim %>% shade_pvalue(1, "right"), 
               "\\`shade_pvalue\\(\\)\\` as a layer")
  expect_error(gss_viz_sim %>% shade_pvalue(obs_stat = 1), 
               "\\`shade_pvalue\\(\\)\\` as a layer")
  expect_error(gss_viz_sim %>% shade_pvalue(obs_stat = 1, 
                                              direction = "right"), 
               "\\`shade_pvalue\\(\\)\\` as a layer")
  
})


# norm_direction ----------------------------------------------------------
test_that("norm_direction works", {
  skip_if(getRversion() > "4.0.2")
  
  expect_equal(norm_direction("left"), "left")
  expect_equal(norm_direction("less"), "left")
  expect_equal(norm_direction("right"), "right")
  expect_equal(norm_direction("greater"), "right")
  expect_equal(norm_direction("both"), "both")
  expect_equal(norm_direction("two-sided"), "both")
  expect_equal(norm_direction("two_sided"), "both")
  expect_equal(norm_direction("two sided"), "both")
})
