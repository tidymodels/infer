context("visualize")

library(dplyr)

set.seed(42)

hours_resamp <- gss_tbl %>%
  specify(hours ~ NULL) %>%
  hypothesize(null = "point", med = 3) %>%
  generate(reps = 10, type = "bootstrap") %>%
  calculate(stat = "median")

obs_slope <- lm(age ~ hours, data = gss_tbl) %>%
  broom::tidy() %>%
  dplyr::filter(term == "hours") %>%
  dplyr::select(estimate) %>%
  dplyr::pull()

obs_diff <- gss_tbl %>%
  group_by(college) %>%
  summarize(prop = mean(college == "no degree")) %>%
  summarize(diff(prop)) %>%
  pull()

obs_z <- sqrt(
  stats::prop.test(
    x = table(gss_tbl$college, gss_tbl$sex),
    n = nrow(gss_tbl),
    alternative = "two.sided",
    correct = FALSE
  )$statistic
)

obs_diff_mean <- gss_tbl %>%
  group_by(college) %>%
  summarize(mean_sepal_width = mean(hours)) %>%
  summarize(diff(mean_sepal_width)) %>%
  pull()

obs_t <- gss_tbl %>%
  t_stat(hours ~ college, order = c("no degree", "degree"))

obs_F <- anova(
    aov(formula = hours ~ partyid, data = gss_tbl)
  )$`F value`[1]

test_that("visualize basic tests", {
  skip_if(getRversion() > "4.0.2")
  
  expect_doppelganger("visualize", visualize(hours_resamp))
  
  # visualise also works
  expect_doppelganger("visualise", visualise(hours_resamp))
  
  expect_error(hours_resamp %>% visualize(bins = "yep"))
  expect_doppelganger(
    "vis-sim-right-1",
    expect_warning(
      gss_tbl %>%
        specify(age ~ hours) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "slope") %>%
        visualize(obs_stat = obs_slope, direction = "right"),
      "deprecated"
    )
  )

  # obs_stat not specified
  expect_error(
    gss_tbl %>%
      specify(sex ~ college, success = "female") %>%
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "diff in props", order = c("no degree", "degree")) %>%
      visualize(direction = "both")
  )

  expect_doppelganger(
    "vis-sim-both-1",
    expect_warning(
      gss_tbl %>%
        specify(sex ~ college, success = "female") %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "diff in props", order = c("no degree", "degree")) %>%
        visualize(direction = "both", obs_stat = obs_diff),
      "deprecated"
    )
  )

  expect_doppelganger(
    "vis-theor-none-1",
    expect_warning(
      gss_tbl %>%
        specify(sex ~ college, success = "female") %>%
        hypothesize(null = "independence") %>%
        calculate(stat = "z", order = c("no degree", "degree")) %>%
        visualize(method = "theoretical")
    )
  )

  # diff in props and z on different scales
  expect_error(
    expect_warning(
      gss_tbl %>%
        specify(sex ~ college, success = "female") %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "diff in props", order = c("no degree", "degree")) %>%
        visualize(method = "both", direction = "both", obs_stat = obs_diff)
    )
  )

  expect_doppelganger(
    "vis-sim-none-1",
    expect_silent(
      gss_tbl %>%
        specify(sex ~ college, success = "female") %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "diff in props", order = c("no degree", "degree")) %>%
        visualize()
    )
  )

  expect_doppelganger(
    "vis-both-both-1",
    expect_warning(
      gss_tbl %>%
        specify(sex ~ college, success = "female") %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "z", order = c("no degree", "degree")) %>%
        visualize(method = "both", direction = "both", obs_stat = obs_z)
    )
  )

  expect_doppelganger(
    "vis-both-both-2",
    expect_warning(
      gss_tbl %>%
        specify(sex ~ college, success = "female") %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "z", order = c("degree", "no degree")) %>%
        visualize(method = "both", direction = "both", obs_stat = -obs_z)
    )
  )

  expect_doppelganger(
    "vis-both-left-1",
    expect_warning(
      gss_tbl %>%
        specify(age ~ sex) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "t", order = c("female", "male")) %>%
        visualize(method = "both", direction = "left", obs_stat = obs_t)
    )
  )

  expect_doppelganger(
    "vis-theor-left-1",
    expect_warning(
      gss_tbl %>%
        specify(age ~ sex) %>%
        hypothesize(null = "independence") %>%
#         generate(reps = 100, type = "permute") %>%
        calculate(stat = "t", order = c("female", "male")) %>%
        visualize(method = "theoretical", direction = "left", obs_stat = obs_t)
    )
  )
  
  expect_doppelganger(
    "vis-both-none-1",
    expect_warning(
      gss_tbl %>%
        specify(hours ~ NULL) %>%
        hypothesize(null = "point", mu = 1) %>%
        generate(reps = 100) %>%
        calculate(stat = "t") %>%
        visualize(method = "both")
    )
  )

  expect_doppelganger(
    "vis-theor-none-2",
    expect_warning(
      gss_tbl %>%
        specify(age ~ college) %>%
        hypothesize(null = "independence") %>%
        visualize(method = "theoretical")
    )
  )

  expect_doppelganger(
    "vis-theor-none-3",
    expect_warning(
      gss_tbl %>%
        specify(age ~ partyid) %>%
        hypothesize(null = "independence") %>%
        visualize(method = "theoretical")
    )
  )

  expect_doppelganger(
    "vis-both-right-1",
    expect_warning(
      gss_tbl %>%
        specify(age ~ partyid) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "F") %>%
        visualize(method = "both", obs_stat = obs_F, direction = "right")
    )
  )

  expect_doppelganger(
    "vis-both-left-2",
    expect_warning(
      gss_tbl %>%
        specify(sex ~ college, success = "female") %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "z", order = c("no degree", "degree")) %>%
        visualize(method = "both", direction = "left", obs_stat = obs_z)
    )
  )

  expect_doppelganger(
    "vis-both-right-2",
    expect_warning(
      gss_tbl %>%
        specify(sex ~ partyid, success = "female") %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "Chisq") %>%
        visualize(method = "both", obs_stat = obs_F, direction = "right")
    )
  )

  expect_doppelganger(
    "vis-theor-right-1",
    expect_warning(
      gss_tbl %>%
        specify(sex ~ partyid, success = "female") %>%
        hypothesize(null = "independence") %>%
#         calculate(stat = "Chisq") %>%
        visualize(method = "theoretical", 
                  obs_stat = obs_F, 
                  direction = "right")
    )
  )

  expect_doppelganger(
    "vis-both-none-2",
    expect_warning(
      gss_tbl %>%
        specify(partyid ~ NULL) %>%
        hypothesize(
          null = "point",
          p = c("dem" = 0.4, "rep" = 0.4, "ind" = 0.2)
        ) %>%
        generate(reps = 100, type = "simulate") %>%
        calculate(stat = "Chisq") %>%
        visualize(method = "both")
    )
  )

  # traditional instead of theoretical
  expect_error(
    gss_tbl %>%
      specify(partyid ~ NULL) %>%
      hypothesize(
        null = "point",
        p = c("dem" = 0.4, "rep" = 0.4, "ind" = 0.2)
      ) %>%
#       generate(reps = 100, type = "simulate") %>%
#       calculate(stat = "Chisq") %>%
      visualize(method = "traditional")
  )

  expect_doppelganger(
    "vis-theor-none-4",
    expect_warning(
      gss_tbl %>%
        specify(partyid ~ NULL) %>%
        hypothesize(
          null = "point",
          p = c("dem" = 0.4, "rep" = 0.4, "ind" = 0.2)
        ) %>%
#         generate(reps = 100, type = "simulate") %>%
#         calculate(stat = "Chisq") %>%
        visualize(method = "theoretical")
    )
  )

  expect_doppelganger(
    "vis-sim-both-2",
    expect_warning(
      gss_tbl %>%
        specify(hours ~ sex) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 10, type = "permute") %>%
        calculate(stat = "diff in means", order = c("female", "male")) %>%
        visualize(direction = "both", obs_stat = obs_diff_mean),
      "deprecated"
    )
  )

  # Produces warning first for not checking conditions but would also error
  expect_error(
    expect_warning(
      gss_tbl %>%
        specify(hours ~ sex) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "diff in means", 
                  order = c("female", "male")) %>%
        visualize(method = "both", 
                  direction = "both", 
                  obs_stat = obs_diff_mean)
    )
  )

  expect_doppelganger(
    "vis-theor-both-1",
    expect_warning(
      gss_tbl %>%
        specify(hours ~ sex) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "diff in means", order = c("female", "male")) %>%
        visualize(
          method = "theoretical", 
          direction = "both",
          obs_stat = obs_diff_mean
        )
    )
  )

  expect_doppelganger(
    "vis-theor-both-2",
    expect_warning(
      gss_tbl %>%
        specify(sex ~ NULL, success = "female") %>%
        hypothesize(null = "point", p = 0.8) %>%
#         generate(reps = 100, type = "simulate") %>%
#         calculate(stat = "z") %>%
        visualize(
          method = "theoretical",
          obs_stat = 2, # Should probably update
          direction = "both"
        )
    )
  )

  expect_doppelganger(
    "vis-sim-left-1",
    expect_warning(
      gss_tbl %>%
        specify(hours ~ NULL) %>%
        hypothesize(null = "point", mu = 1.3) %>%
        generate(reps = 100, type = "bootstrap") %>%
        calculate(stat = "mean") %>%
        visualize(direction = "left", obs_stat = mean(gss_tbl$hours)),
      "deprecated"
    )
  )
})

test_that("mirror_obs_stat works", {
  skip_if(getRversion() > "4.0.2")
  
  expect_equal(mirror_obs_stat(1:10, 4), c(`60%` = 6.4))
})

test_that("obs_stat as a data.frame works", {
  skip_if(getRversion() > "4.0.2")
  
  mean_petal_width <- gss_tbl %>%
    specify(hours ~ NULL) %>%
    calculate(stat = "mean")
  expect_doppelganger(
    "df-obs_stat-1",
    expect_warning(
      gss_tbl %>%
        specify(hours ~ NULL) %>%
        hypothesize(null = "point", mu = 4) %>%
        generate(reps = 100, type = "bootstrap") %>%
        calculate(stat = "mean") %>%
        visualize(obs_stat = mean_petal_width),
      "deprecated"
    )
  )
  
  mean_df_test <- data.frame(x = c(4.1, 1), y = c(1, 2))
  expect_doppelganger(
    "df-obs_stat-2",
    expect_warning(
      gss_tbl %>%
        specify(hours ~ NULL) %>%
        hypothesize(null = "point", mu = 4) %>%
        generate(reps = 100, type = "bootstrap") %>%
        calculate(stat = "mean") %>%
        visualize(obs_stat = mean_df_test)
    )
  )
})

test_that('method = "both" behaves nicely', {
  skip_if(getRversion() > "4.0.2")
  # stop_glue(
  #   '`generate()` and `calculate()` are both required to be done prior ',
  #   'to `visualize(method = "both")`'
  # )
  expect_error(
    gss_tbl %>%
      specify(hours ~ NULL) %>%
      hypothesize(null = "point", mu = 4) %>%
      generate(reps = 100, type = "bootstrap") %>%
#       calculate(stat = "mean") %>%
      visualize(method = "both")
  )

  expect_doppelganger(
    "method-both",
    expect_warning(
      gss_tbl %>%
        specify(hours ~ college) %>%
        hypothesize(null = "point", mu = 4) %>%
        generate(reps = 10, type = "bootstrap") %>%
        calculate(stat = "t", order = c("no degree", "degree")) %>%
        visualize(method = "both")
    )
  )
})

test_that("Traditional right-tailed tests have warning if not right-tailed", {
  skip_if(getRversion() > "4.0.2")
  
  expect_warning(
    gss_tbl %>%
      specify(sex ~ partyid, success = "female") %>%
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "Chisq") %>%
      visualize(method = "both", obs_stat = 2, direction = "left")
  )
  expect_warning(
    gss_tbl %>%
      specify(age ~ partyid) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "F") %>%
      visualize(method = "both", obs_stat = 2, direction = "two_sided")
  )
  expect_warning(
    gss_tbl %>%
      specify(sex ~ partyid, success = "female") %>%
      hypothesize(null = "independence") %>%
#       generate(reps = 100, type = "permute") %>%
      calculate(stat = "Chisq") %>%
      visualize(method = "theoretical", obs_stat = 2, direction = "left")
  )
  expect_warning(
    gss_tbl %>%
      specify(age ~ partyid) %>%
      hypothesize(null = "independence") %>%
#       generate(reps = 100, type = "permute") %>%
      calculate(stat = "F") %>%
      visualize(method = "theoretical", obs_stat = 2, direction = "two_sided")
  )
})

test_that("confidence interval plots are working", {
  skip_if(getRversion() > "4.0.2")
  
  gss_tbl_boot <- gss_tbl %>%
    specify(sex ~ college, success = "female") %>%
    generate(reps = 100) %>%
    calculate(stat = "diff in props", order = c("no degree", "degree"))

  df_error <- tibble::tibble(col1 = rnorm(5), col2 = rnorm(5))
  vec_error <- 1:10

  perc_ci <- gss_tbl_boot %>% get_ci()

  expect_error(gss_tbl_boot %>% visualize(endpoints = df_error))

  expect_warning(gss_tbl_boot %>% visualize(endpoints = vec_error))

  expect_doppelganger(
    "ci-vis",
    expect_warning(
      gss_tbl_boot %>% visualize(endpoints = perc_ci, direction = "between"),
      "deprecated"
    )
  )

  expect_warning(gss_tbl_boot %>% visualize(obs_stat = 3, endpoints = perc_ci))
})

test_that("title adapts to not hypothesis testing workflow", {
  skip_if(getRversion() > "4.0.2")
  
  set.seed(100)
  gss_tbl_boot_tbl <- gss_tbl %>% 
    specify(response = hours) %>% 
    generate(reps = 100, type = "bootstrap")
  
  expect_doppelganger(
    "vis-no-hypothesize-sim",
    gss_tbl_boot_tbl %>% 
      calculate(stat = "mean") %>%
      visualize()
  )
  expect_doppelganger(
    "vis-no-hypothesize-both",
    expect_warning(
      gss_tbl_boot_tbl %>% 
        calculate(stat = "t") %>%
        visualize(method = "both")
    )
  )
})

test_that("warn_right_tail_test works", {
  skip_if(getRversion() > "4.0.2")
  
  expect_warn_right_tail <- function(stat_name) {
    warn_regex <- paste0(stat_name, ".*right-tailed")
    
    expect_silent(warn_right_tail_test(NULL, stat_name))
    expect_silent(warn_right_tail_test("right", stat_name))
    expect_warning(warn_right_tail_test("left", stat_name), warn_regex)
    expect_warning(warn_right_tail_test("two_sided", stat_name), warn_regex)
  }
  
  expect_warn_right_tail("F")
  expect_warn_right_tail("Chi-Square")
})

test_that("visualize warns about removing `NaN`", {
  skip_if(getRversion() > "4.0.2")
  
  dist <- gss_tbl_boot_tbl <- gss_tbl %>% 
    specify(response = hours) %>% 
    generate(reps = 10, type = "bootstrap") %>% 
    calculate("mean")
  
  # A warning should be raised if there is NaN in a visualized dist
  dist$stat[1] <- NaN
  expect_warning(visualize(dist), "1 calculated statistic was")
  
  # And a different warning for plural NaNs
  dist$stat[2] <- NaN
  expect_warning(visualize(dist), "2 calculated statistics were")
  
  # In the case that _all_ values are NaN, error should be raised
  dist$stat <- rep(NaN, nrow(dist))
  expect_error(visualize(dist), "All calculated stat")
})
