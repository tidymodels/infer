library(dplyr)

set.seed(42)

hours_resamp <- gss_tbl |>
  specify(hours ~ NULL) |>
  hypothesize(null = "point", med = 3) |>
  generate(reps = 10, type = "bootstrap") |>
  calculate(stat = "median")

obs_slope <- lm(age ~ hours, data = gss_tbl) |>
  broom::tidy() |>
  dplyr::filter(term == "hours") |>
  dplyr::select(estimate) |>
  dplyr::pull()

obs_diff <- gss_tbl |>
  group_by(college) |>
  summarize(prop = mean(college == "no degree")) |>
  summarize(diff(prop)) |>
  pull()

obs_z <- sqrt(
  stats::prop.test(
    x = table(gss_tbl$college, gss_tbl$sex),
    n = nrow(gss_tbl),
    alternative = "two.sided",
    correct = FALSE
  )$statistic
)

obs_diff_mean <- gss_tbl |>
  group_by(college) |>
  summarize(mean_sepal_width = mean(hours)) |>
  summarize(diff(mean_sepal_width)) |>
  pull()

obs_t <- gss_tbl |>
  observe(hours ~ college, order = c("no degree", "degree"), stat = "t")

obs_F <- anova(
  aov(formula = hours ~ partyid, data = gss_tbl)
)$`F value`[1]

test_that("visualize warns with bad arguments", {
  skip_if(getRversion() < "4.1.0")

  # warns when supplied deprecated args in what used to be
  # a valid way
  expect_snapshot(
    res_ <- gss_tbl |>
      specify(age ~ hours) |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "slope") |>
      visualize(obs_stat = obs_slope, direction = "right")
  )

  # warning is the same when deprecated args are inappropriate
  expect_snapshot(
    res_ <- gss_tbl |>
      specify(age ~ hours) |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "slope") |>
      visualize(obs_stat = obs_slope)
  )

  # same goes for CI args
  expect_snapshot(
    res_ <- gss_tbl |>
      specify(age ~ hours) |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "slope") |>
      visualize(endpoints = c(.01, .02))
  )

  # output should not change when supplied a deprecated argument
  age_hours_df <- gss_tbl |>
    specify(age ~ hours) |>
    hypothesize(null = "independence") |>
    generate(reps = 100, type = "permute") |>
    calculate(stat = "slope")

  expect_snapshot(
    res <- age_hours_df |>
      visualize(endpoints = c(.01, .02))
  )

  expect_equal(
    age_hours_df |>
      visualize(),
    res
  )
})

test_that("visualize basic tests", {
  skip_if(getRversion() < "4.1.0")

  expect_doppelganger("visualize", visualize(hours_resamp))

  # visualise also works
  expect_doppelganger("visualise", visualise(hours_resamp))

  expect_snapshot(error = TRUE, hours_resamp |> visualize(bins = "yep"))
  expect_doppelganger(
    "vis-sim-right-1",
    gss_tbl |>
      specify(age ~ hours) |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "slope") |>
      visualize() +
      shade_p_value(obs_stat = obs_slope, direction = "right")
  )

  # obs_stat not specified
  expect_snapshot_error(
    gss_tbl |>
      specify(sex ~ college, success = "female") |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "diff in props", order = c("no degree", "degree")) |>
      visualize() +
      shade_p_value(direction = "both")
  )

  expect_doppelganger(
    "vis-sim-both-1",
    gss_tbl |>
      specify(sex ~ college, success = "female") |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "diff in props", order = c("no degree", "degree")) |>
      visualize() +
      shade_p_value(direction = "both", obs_stat = obs_diff)
  )

  expect_snapshot(
    res_vis_theor_none_1 <- gss_tbl |>
      specify(sex ~ college, success = "female") |>
      hypothesize(null = "independence") |>
      calculate(stat = "z", order = c("no degree", "degree")) |>
      visualize(method = "theoretical")
  )

  expect_doppelganger("vis-theor-none-1", res_vis_theor_none_1)

  # diff in props and z on different scales
  expect_snapshot(
    error = TRUE,
    gss_tbl |>
      specify(sex ~ college, success = "female") |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "diff in props", order = c("no degree", "degree")) |>
      visualize(method = "both") +
      shade_p_value(direction = "both", obs_stat = obs_diff)
  )

  expect_doppelganger(
    "vis-sim-none-1",
    expect_silent(
      gss_tbl |>
        specify(sex ~ college, success = "female") |>
        hypothesize(null = "independence") |>
        generate(reps = 100, type = "permute") |>
        calculate(stat = "diff in props", order = c("no degree", "degree")) |>
        visualize()
    )
  )

  expect_warning(
    vis_both_both_1 <- gss_tbl |>
      specify(sex ~ college, success = "female") |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "z", order = c("no degree", "degree")) |>
      visualize(method = "both") +
      shade_p_value(direction = "both", obs_stat = obs_z)
  )
  expect_doppelganger(
    "vis-both-both-1",
    vis_both_both_1
  )

  expect_warning(
    vis_both_both_2 <- gss_tbl |>
      specify(sex ~ college, success = "female") |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "z", order = c("degree", "no degree")) |>
      visualize(method = "both") +
      shade_p_value(direction = "both", obs_stat = -obs_z)
  )
  expect_doppelganger(
    "vis-both-both-2",
    vis_both_both_2
  )

  expect_warning(
    vis_both_left_1 <- gss_tbl |>
      specify(age ~ sex) |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "t", order = c("female", "male")) |>
      visualize(method = "both") +
      shade_p_value(direction = "left", obs_stat = obs_t)
  )
  expect_doppelganger(
    "vis-both-left-1",
    vis_both_left_1
  )

  expect_warning(
    vis_theor_left_1 <- gss_tbl |>
      specify(age ~ sex) |>
      hypothesize(null = "independence") |>
      # generate(reps = 100, type = "permute") |>
      calculate(stat = "t", order = c("female", "male")) |>
      visualize(method = "theoretical") +
      shade_p_value(direction = "left", obs_stat = obs_t)
  )
  expect_doppelganger(
    "vis-theor-left-1",
    vis_theor_left_1
  )

  expect_warning(
    vis_both_none_1 <- gss_tbl |>
      specify(hours ~ NULL) |>
      hypothesize(null = "point", mu = 1) |>
      generate(reps = 100) |>
      calculate(stat = "t") |>
      visualize(method = "both")
  )
  expect_doppelganger(
    "vis-both-none-1",
    vis_both_none_1
  )

  expect_warning(
    vis_theor_none_2 <- gss_tbl |>
      specify(age ~ college) |>
      hypothesize(null = "independence") |>
      visualize(method = "theoretical")
  )
  expect_doppelganger(
    "vis-theor-none-2",
    vis_theor_none_2
  )

  expect_warning(
    vis_theor_none_3 <- gss_tbl |>
      specify(age ~ partyid) |>
      hypothesize(null = "independence") |>
      visualize(method = "theoretical")
  )
  expect_doppelganger(
    "vis-theor-none-3",
    vis_theor_none_3
  )

  expect_warning(
    vis_both_right_1 <- gss_tbl |>
      specify(age ~ partyid) |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "F") |>
      visualize(method = "both") +
      shade_p_value(obs_stat = obs_F, direction = "right")
  )
  expect_doppelganger(
    "vis-both-right-1",
    vis_both_right_1
  )

  expect_warning(
    vis_both_left_2 <- gss_tbl |>
      specify(sex ~ college, success = "female") |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "z", order = c("no degree", "degree")) |>
      visualize(method = "both") +
      shade_p_value(direction = "left", obs_stat = obs_z)
  )
  expect_doppelganger(
    "vis-both-left-2",
    vis_both_left_2
  )

  expect_warning(
    vis_both_right_2 <- gss_tbl |>
      specify(sex ~ partyid, success = "female") |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "Chisq") |>
      visualize(method = "both") +
      shade_p_value(obs_stat = obs_F, direction = "right")
  )
  expect_doppelganger(
    "vis-both-right-2",
    vis_both_right_2
  )

  expect_warning(
    vis_theor_right_1 <- gss_tbl |>
      specify(sex ~ partyid, success = "female") |>
      hypothesize(null = "independence") |>
      # alculate(stat = "Chisq") |>
      visualize(method = "theoretical") +
      shade_p_value(obs_stat = obs_F, direction = "right")
  )
  expect_doppelganger(
    "vis-theor-right-1",
    vis_theor_right_1
  )

  expect_warning(
    vis_both_none_2 <- gss_tbl |>
      specify(partyid ~ NULL) |>
      hypothesize(
        null = "point",
        p = c("dem" = 0.4, "rep" = 0.4, "ind" = 0.2)
      ) |>
      generate(reps = 100, type = "draw") |>
      calculate(stat = "Chisq") |>
      visualize(method = "both")
  )
  expect_doppelganger(
    "vis-both-none-2",
    vis_both_none_2
  )

  # traditional instead of theoretical
  expect_snapshot(
    error = TRUE,
    gss_tbl |>
      specify(partyid ~ NULL) |>
      hypothesize(
        null = "point",
        p = c("dem" = 0.4, "rep" = 0.4, "ind" = 0.2)
      ) |>
      #       generate(reps = 100, type = "draw") |>
      #       calculate(stat = "Chisq") |>
      visualize(method = "traditional")
  )

  expect_warning(
    vis_theor_none_4 <- gss_tbl |>
      specify(partyid ~ NULL) |>
      hypothesize(
        null = "point",
        p = c("dem" = 0.4, "rep" = 0.4, "ind" = 0.2)
      ) |>
      #         generate(reps = 100, type = "draw") |>
      #         calculate(stat = "Chisq") |>
      visualize(method = "theoretical")
  )
  expect_doppelganger(
    "vis-theor-none-4",
    vis_theor_none_4
  )

  expect_doppelganger(
    "vis-sim-both-2",
    gss_tbl |>
      specify(hours ~ sex) |>
      hypothesize(null = "independence") |>
      generate(reps = 10, type = "permute") |>
      calculate(stat = "diff in means", order = c("female", "male")) |>
      visualize() +
      shade_p_value(direction = "both", obs_stat = obs_diff_mean)
  )

  # Produces warning first for not checking conditions but would also error
  expect_snapshot(
    error = TRUE,
    gss_tbl |>
      specify(hours ~ sex) |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "diff in means", order = c("female", "male")) |>
      visualize(method = "both") +
      shade_p_value(direction = "both", obs_stat = obs_diff_mean)
  )

  expect_snapshot(
    res_vis_theor_both_1 <- gss_tbl |>
      specify(hours ~ sex) |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "diff in means", order = c("female", "male")) |>
      visualize(method = "theoretical") +
      shade_p_value(direction = "both", obs_stat = obs_diff_mean)
  )

  expect_doppelganger("vis-theor-both-1", res_vis_theor_both_1)

  expect_warning(
    vis_theor_both_2 <- gss_tbl |>
      specify(sex ~ NULL, success = "female") |>
      hypothesize(null = "point", p = 0.8) |>
      #         generate(reps = 100, type = "draw") |>
      #         calculate(stat = "z") |>
      visualize(method = "theoretical") +
      shade_p_value(obs_stat = 2, direction = "both")
  )
  expect_doppelganger(
    "vis-theor-both-2",
    vis_theor_both_2
  )

  expect_doppelganger(
    "vis-sim-left-1",
    gss_tbl |>
      specify(hours ~ NULL) |>
      hypothesize(null = "point", mu = 1.3) |>
      generate(reps = 100, type = "bootstrap") |>
      calculate(stat = "mean") |>
      visualize() +
      shade_p_value(direction = "left", obs_stat = mean(gss_tbl$hours))
  )
})

test_that("mirror_obs_stat works", {
  skip_if(getRversion() < "4.1.0")

  expect_equal(mirror_obs_stat(1:10, 4), c(`60%` = 6.4))
})

test_that("obs_stat as a data.frame works", {
  skip_if(getRversion() < "4.1.0")

  mean_petal_width <- gss_tbl |>
    specify(hours ~ NULL) |>
    calculate(stat = "mean")
  expect_doppelganger(
    "df-obs_stat-1",
    gss_tbl |>
      specify(hours ~ NULL) |>
      hypothesize(null = "point", mu = 4) |>
      generate(reps = 100, type = "bootstrap") |>
      calculate(stat = "mean") |>
      visualize() +
      shade_p_value(obs_stat = mean_petal_width, direction = "both")
  )

  mean_df_test <- data.frame(x = c(4.1, 1), y = c(1, 2))
  expect_warning(
    df_obs_stat_2 <- gss_tbl |>
      specify(hours ~ NULL) |>
      hypothesize(null = "point", mu = 4) |>
      generate(reps = 100, type = "bootstrap") |>
      calculate(stat = "mean") |>
      visualize() +
      shade_p_value(obs_stat = mean_df_test, direction = "both")
  )
  expect_doppelganger(
    "df-obs_stat-2",
    df_obs_stat_2
  )
})

test_that('method = "both" behaves nicely', {
  skip_if(getRversion() < "4.1.0")

  expect_snapshot(
    error = TRUE,
    gss_tbl |>
      specify(hours ~ NULL) |>
      hypothesize(null = "point", mu = 4) |>
      generate(reps = 100, type = "bootstrap") |>
      #       calculate(stat = "mean") |>
      visualize(method = "both")
  )

  expect_snapshot(
    res_method_both <- gss_tbl |>
      specify(hours ~ college) |>
      hypothesize(null = "point", mu = 4) |>
      generate(reps = 10, type = "bootstrap") |>
      calculate(stat = "t", order = c("no degree", "degree")) |>
      visualize(method = "both")
  )

  expect_doppelganger("method-both", res_method_both)
})

test_that("Traditional right-tailed tests have warning if not right-tailed", {
  skip_if(getRversion() < "4.1.0")
  withr::local_envvar(SUPPRESS_INFER_MESSAGES = "false")

  expect_snapshot(
    res_ <- gss_tbl |>
      specify(sex ~ partyid, success = "female") |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "Chisq") |>
      visualize(method = "both") +
      shade_p_value(obs_stat = 2, direction = "left")
  )

  expect_snapshot(
    res_ <- gss_tbl |>
      specify(age ~ partyid) |>
      hypothesize(null = "independence") |>
      generate(reps = 100, type = "permute") |>
      calculate(stat = "F") |>
      visualize(method = "both") +
      shade_p_value(obs_stat = 2, direction = "two_sided")
  )

  expect_snapshot(
    res_ <- gss_tbl |>
      specify(sex ~ partyid, success = "female") |>
      hypothesize(null = "independence") |>
      #       generate(reps = 100, type = "permute") |>
      calculate(stat = "Chisq") |>
      visualize(method = "theoretical") +
      shade_p_value(obs_stat = 2, direction = "left")
  )

  expect_snapshot(
    res_ <- gss_tbl |>
      specify(age ~ partyid) |>
      hypothesize(null = "independence") |>
      #       generate(reps = 100, type = "permute") |>
      calculate(stat = "F") |>
      visualize(method = "theoretical") +
      shade_p_value(obs_stat = 2, direction = "two_sided")
  )
})

test_that("confidence interval plots are working", {
  skip_if(getRversion() < "4.1.0")

  gss_tbl_boot <- gss_tbl |>
    specify(sex ~ college, success = "female") |>
    generate(reps = 100) |>
    calculate(stat = "diff in props", order = c("no degree", "degree"))

  df_error <- tibble::tibble(col1 = rnorm(5), col2 = rnorm(5))
  vec_error <- 1:10

  perc_ci <- gss_tbl_boot |> get_ci()

  expect_snapshot(
    error = TRUE,
    res_ <- gss_tbl_boot |>
      visualize() +
      shade_confidence_interval(endpoints = df_error)
  )

  expect_snapshot(
    res_ <- gss_tbl_boot |>
      visualize() +
      shade_confidence_interval(endpoints = vec_error)
  )

  expect_snapshot(
    res_ci_vis <- gss_tbl_boot |>
      visualize() +
      shade_confidence_interval(endpoints = perc_ci, direction = "between")
  )

  expect_doppelganger("ci-vis", res_ci_vis)
})

test_that("title adapts to not hypothesis testing workflow", {
  skip_if(getRversion() < "4.1.0")

  set.seed(100)
  gss_tbl_boot_tbl <- gss_tbl |>
    specify(response = hours) |>
    generate(reps = 100, type = "bootstrap")

  expect_doppelganger(
    "vis-no-hypothesize-sim",
    gss_tbl_boot_tbl |>
      calculate(stat = "mean") |>
      visualize()
  )
  expect_snapshot(
    res_vis_no_hypothesize_both <- gss_tbl_boot_tbl |>
      calculate(stat = "t") |>
      visualize(method = "both")
  )

  expect_doppelganger("vis-no-hypothesize-both", res_vis_no_hypothesize_both)
})

test_that("warn_right_tail_test works", {
  skip_if(getRversion() < "4.1.0")

  expect_warn_right_tail <- function(stat_name) {
    expect_silent(warn_right_tail_test(NULL, stat_name))
    expect_silent(warn_right_tail_test("right", stat_name))
    expect_snapshot(warn_right_tail_test("left", stat_name))
    expect_snapshot(warn_right_tail_test("two_sided", stat_name))
  }

  expect_warn_right_tail("F")
  expect_warn_right_tail("Chi-Square")
})

test_that("visualize warns about removing `NaN`", {
  skip_if(getRversion() < "4.1.0")

  dist <- gss_tbl_boot_tbl <- gss_tbl |>
    specify(response = hours) |>
    generate(reps = 10, type = "bootstrap") |>
    calculate("mean")

  # A warning should be raised if there is NaN in a visualized dist
  dist$stat[1] <- NaN
  expect_snapshot(res_ <- visualize(dist))

  # And a different warning for plural NaNs
  dist$stat[2] <- NaN
  expect_snapshot(res_ <- visualize(dist))

  # In the case that _all_ values are NaN, error should be raised
  dist$stat <- rep(NaN, nrow(dist))
  expect_snapshot(error = TRUE, res_ <- visualize(dist))
})

test_that("visualize can handle multiple explanatory variables", {
  skip_if(getRversion() < "4.1.0")
  skip_if_not(identical(Sys.info()[["sysname"]], "Darwin"))

  # generate example objects
  null_fits <- gss |>
    specify(hours ~ age + college) |>
    hypothesize(null = "independence") |>
    generate(reps = 20, type = "permute") |>
    fit()

  obs_fit <- gss |>
    specify(hours ~ age + college) |>
    fit()

  conf_ints <-
    get_confidence_interval(
      null_fits,
      point_estimate = obs_fit,
      level = .95
    )

  # visualize with multiple panes
  expect_doppelganger(
    "viz-fit-bare",
    null_fits |>
      visualize()
  )

  # with p values shaded -- test each possible direction
  expect_doppelganger(
    "viz-fit-p-val-both",
    null_fits |>
      visualize() +
      shade_p_value(obs_stat = obs_fit, direction = "both")
  )

  expect_doppelganger(
    "viz-fit-p-val-left",
    null_fits |>
      visualize() +
      shade_p_value(obs_stat = obs_fit, direction = "left")
  )

  expect_snapshot(
    res_viz_fit_p_val_right <-
      null_fits |>
      visualize() +
      shade_p_value(obs_stat = obs_fit, direction = "right")
  )

  expect_doppelganger(
    "viz-fit-p-val-right",
    res_viz_fit_p_val_right
  )

  # with confidence intervals shaded
  expect_doppelganger(
    "viz-fit-conf-int",
    null_fits |>
      visualize() +
      shade_confidence_interval(endpoints = conf_ints)
  )

  # with no hypothesize()
  expect_doppelganger(
    "viz-fit-no-h0",
    gss |>
      specify(hours ~ age + college) |>
      generate(reps = 20, type = "bootstrap") |>
      fit() |>
      visualize()
  )

  # shade_* functions should error with bad input
})

test_that("visualize can handle `assume()` output", {
  skip_if(getRversion() < "4.1.0")

  # F ----------------------------------------------------------------------
  obs_stat <- gss |>
    specify(age ~ partyid) |>
    calculate(stat = "F")

  null_dist <- gss |>
    specify(age ~ partyid) |>
    hypothesize(null = "independence") |>
    assume(distribution = "F")

  expect_doppelganger(
    "viz-assume-f",
    visualize(null_dist)
  )

  expect_doppelganger(
    "viz-assume-f-p-val",
    visualize(null_dist) + shade_p_value(obs_stat, "right")
  )

  # t (mean) -----------------------------------------------------------------
  obs_stat <- gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 40) |>
    calculate(stat = "t")

  null_dist <- gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 40) |>
    assume("t")

  obs_mean <- gss |>
    specify(response = hours) |>
    calculate(stat = "mean")

  ci <-
    get_confidence_interval(
      null_dist,
      level = .95,
      point_estimate = obs_mean
    )

  expect_doppelganger(
    "viz-assume-t",
    visualize(null_dist)
  )

  expect_doppelganger(
    "viz-assume-t-p-val-both",
    visualize(null_dist) + shade_p_value(obs_stat, "both")
  )

  expect_doppelganger(
    "viz-assume-t-p-val-left",
    visualize(null_dist) + shade_p_value(obs_stat, "left")
  )

  expect_doppelganger(
    "viz-assume-t-p-val-right",
    visualize(null_dist) + shade_p_value(obs_stat, "right")
  )

  expect_doppelganger(
    "viz-assume-t-ci",
    visualize(null_dist) + shade_confidence_interval(ci)
  )

  # warns when it ought to --------------------------------------------------
  expect_snapshot(
    res_viz_assume_t_sim <- visualize(null_dist, method = "simulation")
  )
  expect_doppelganger(
    "viz-assume-t-sim",
    res_viz_assume_t_sim
  )

  expect_snapshot(
    res_viz_assume_t_both <- visualize(null_dist, method = "both")
  )

  expect_doppelganger(
    "viz-assume-t-both",
    res_viz_assume_t_both
  )

  # t (diff in means) -----------------------------------------------------------------
  obs_stat <- gss |>
    specify(hours ~ college) |>
    calculate(stat = "t", order = c("degree", "no degree"))

  null_dist <- gss |>
    specify(hours ~ college) |>
    hypothesize(null = "independence") |>
    assume("t")

  obs_diff <- gss |>
    specify(hours ~ college) |>
    calculate(stat = "diff in means", order = c("degree", "no degree"))

  ci <-
    get_confidence_interval(
      null_dist,
      level = .95,
      point_estimate = obs_diff
    )

  expect_doppelganger(
    "viz-assume-2t",
    visualize(null_dist)
  )

  expect_doppelganger(
    "viz-assume-2t-p-val-both",
    visualize(null_dist) + shade_p_value(obs_stat, "both")
  )

  expect_doppelganger(
    "viz-assume-2t-p-val-left",
    visualize(null_dist) + shade_p_value(obs_stat, "left")
  )

  expect_doppelganger(
    "viz-assume-2t-p-val-right",
    visualize(null_dist) + shade_p_value(obs_stat, "right")
  )

  expect_doppelganger(
    "viz-assume-2t-ci",
    visualize(null_dist) + shade_confidence_interval(ci)
  )

  # z (prop) -----------------------------------------------------------------
  obs_stat <- gss |>
    specify(response = sex, success = "female") |>
    hypothesize(null = "point", p = .5) |>
    calculate(stat = "z")

  null_dist <- gss |>
    specify(response = sex, success = "female") |>
    hypothesize(null = "point", p = .5) |>
    assume("z")

  obs_prop <- gss |>
    specify(response = sex, success = "female") |>
    calculate(stat = "prop")

  ci <-
    get_confidence_interval(
      null_dist,
      level = .95,
      point_estimate = obs_prop
    )

  expect_doppelganger(
    "viz-assume-z",
    visualize(null_dist)
  )

  expect_doppelganger(
    "viz-assume-z-p-val-both",
    visualize(null_dist) + shade_p_value(obs_stat, "both")
  )

  expect_doppelganger(
    "viz-assume-z-p-val-left",
    visualize(null_dist) + shade_p_value(obs_stat, "left")
  )

  expect_doppelganger(
    "viz-assume-z-p-val-right",
    visualize(null_dist) + shade_p_value(obs_stat, "right")
  )

  expect_doppelganger(
    "viz-assume-z-ci",
    visualize(null_dist) + shade_confidence_interval(ci)
  )

  # z (diff in props) --------------------------------------------------------
  obs_stat <- gss |>
    specify(college ~ sex, success = "no degree") |>
    calculate(stat = "z", order = c("female", "male"))

  null_dist <- gss |>
    specify(college ~ sex, success = "no degree") |>
    hypothesize(null = "independence") |>
    assume("z")

  obs_diff <- gss |>
    specify(college ~ sex, success = "no degree") |>
    calculate(stat = "diff in props", order = c("female", "male"))

  ci <-
    get_confidence_interval(
      null_dist,
      level = .95,
      point_estimate = obs_diff
    )

  expect_doppelganger(
    "viz-assume-2z",
    visualize(null_dist)
  )

  expect_doppelganger(
    "viz-assume-2z-p-val-both",
    visualize(null_dist) + shade_p_value(obs_stat, "both")
  )

  expect_doppelganger(
    "viz-assume-2z-p-val-left",
    visualize(null_dist) + shade_p_value(obs_stat, "left")
  )

  expect_doppelganger(
    "viz-assume-2z-p-val-right",
    visualize(null_dist) + shade_p_value(obs_stat, "right")
  )

  expect_doppelganger(
    "viz-assume-2z-ci",
    visualize(null_dist) + shade_confidence_interval(ci)
  )
})
