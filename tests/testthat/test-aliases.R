context("aliases")

test_that("aliases work", {
  expect_equal(
    gss_calc %>%
      get_pvalue(obs_stat = -0.2, direction = "right") %>%
      dplyr::pull(),
    expected = 1,
    tolerance = eps
  )

  expect_silent(gss_permute %>% get_ci())
})

test_that("old aliases produce warning", {
  expect_warning(
    gss_calc %>%
      p_value(obs_stat = -0.2, direction = "right") %>%
      dplyr::pull(),
    expected = 1
  )

  expect_warning(gss_permute %>% conf_int())
})
