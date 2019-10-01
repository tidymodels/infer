set.seed(4242)

iris_df <- tibble::as_tibble(iris)

iris_tbl <- iris %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    Sepal.Length.Group = dplyr::if_else(Sepal.Length > 5, ">5", "<=5"),
    Sepal.Width.Group = dplyr::if_else(Sepal.Width > 3, "large", "small")
  )

iris_calc <- iris_tbl %>%
  specify(Sepal.Length.Group ~ Sepal.Width.Group, success = "<=5") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000) %>%
  calculate(stat = "diff in props", order = c("large", "small"))

mtcars_df <- mtcars %>%
  dplyr::mutate(
    cyl = factor(cyl), vs = factor(vs), am = factor(am), gear = factor(gear),
    carb = factor(carb)
  )

obs_diff <- iris_tbl %>%
  specify(Sepal.Length.Group ~ Sepal.Width.Group, success = "<=5") %>%
  calculate(stat = "diff in props", order = c("large", "small"))

set.seed(2018)
test_df <- tibble::tibble(stat = rnorm(100))

# Data for visualization tests

iris_permute <- iris_tbl %>%
  specify(Sepal.Width.Group ~ Sepal.Length.Group, success = "large") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "z", order = c(">5", "<=5"))
iris_viz_sim <- iris_permute %>% visualize(method = "simulation")
# Warnings are about checking conditions for the theoretical method.
iris_viz_theor <- suppressWarnings(
  iris_permute %>% visualize(method = "theoretical")
)
iris_viz_both <- suppressWarnings(
  iris_permute %>% visualize(method = "both")
)
