set.seed(4242)

expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}

eps <- if (capabilities("long.double")) {sqrt(.Machine$double.eps)} else {0.01}

gss_tbl <- tibble::as_tibble(gss) %>%
  dplyr::filter(!(is.na(sex) | is.na(college))) %>%
  dplyr::mutate(partyid = as.character(partyid)) %>%
  dplyr::filter(partyid %in% c("ind", "rep", "dem"))

gss_calc <- gss_tbl %>%
  specify(college ~ sex, success = "no degree") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("female", "male"))

mtcars_df <- mtcars %>%
  dplyr::mutate(
    cyl = factor(cyl), vs = factor(vs), am = factor(am), gear = factor(gear),
    carb = factor(carb)
  )

obs_diff <- gss_tbl %>%
  specify(college ~ sex, success = "no degree") %>%
  calculate(stat = "diff in props", order = c("female", "male"))

set.seed(2018)
test_df <- tibble::tibble(stat = rnorm(100))

# Data for visualization tests

gss_permute <- gss_tbl %>%
  specify(college ~ sex, success = "no degree") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "z", order = c("female", "male"))

gss_viz_sim <- gss_permute %>% visualize(method = "simulation")

# Warnings are about checking conditions for the theoretical method.
gss_viz_theor <- suppressWarnings(suppressMessages(
  gss_permute %>% visualize(method = "theoretical")
))
gss_viz_both <- suppressWarnings(
  gss_permute %>% visualize(method = "both")
)
