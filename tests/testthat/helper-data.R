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
