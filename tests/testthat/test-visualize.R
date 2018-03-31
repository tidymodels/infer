context("visualize")

library(dplyr)


Sepal.Width_resamp <- iris %>%
  specify(Sepal.Width ~ NULL) %>%
  hypothesize(null = "point", med = 3) %>%
  generate(reps = 10, type = "bootstrap") %>% 
  calculate(stat = "median") 

iris_tbl <- tibble::as_tibble(iris) %>% 
  dplyr::mutate(Sepal.Length.Group =
                  dplyr::if_else(Sepal.Length > 5, ">5", "<=5"),
                Sepal.Width.Group =
                  dplyr::if_else(Sepal.Width > 3, "large", "small")) 
  
obs_slope <- lm(Sepal.Length ~ Sepal.Width, 
                   data = iris_tbl) %>% 
  broom::tidy() %>% 
  dplyr::filter(term == "Sepal.Width") %>% 
  dplyr::select(estimate) %>% 
  dplyr::pull()

obs_diff <- iris_tbl %>%
  group_by(Sepal.Length.Group) %>%
  summarize(prop = mean(Sepal.Width.Group == ">5")) %>%
  summarize(diff(prop)) %>%
  pull()

obs_diff_mean <- iris_tbl %>%
  group_by(Sepal.Length.Group) %>% 
  summarize(mean_sepal_width = mean(Sepal.Width)) %>%
  summarize(diff(mean_sepal_width)) %>%
  pull()

obs_t <- iris_tbl %>%
  t_stat(Sepal.Width ~ Sepal.Length.Group)

test_that("visualize basic tests", {
  expect_silent(visualize(Sepal.Width_resamp))
  expect_error(
    Sepal.Width_resamp %>% visualize(bins = "yep")
  )
  expect_silent(iris_tbl %>%
                  specify(Sepal.Length ~ Sepal.Width) %>%
                  hypothesize(null = "independence") %>%
                  generate(reps = 10, type = "permute") %>% 
                  calculate(stat = "slope") %>% 
                  visualize(obs_stat = obs_slope, direction = "right"))
  
  #obs_stat not specified
  expect_error(iris_tbl %>% 
                  specify(Sepal.Width.Group ~ Sepal.Length.Group,
                          success = "large") %>% 
                  hypothesize(null = "independence") %>% 
                  generate(reps = 10, type = "permute") %>% 
                  calculate(stat = "diff in props", 
                            order = c(">5", "<=5")) %>% 
                  visualize(direction = "both")
                )
  
  expect_silent(iris_tbl %>% 
                 specify(Sepal.Width.Group ~ Sepal.Length.Group,
                         success = "large") %>% 
                 hypothesize(null = "independence") %>% 
                 generate(reps = 10, type = "permute") %>% 
                 calculate(stat = "diff in props", 
                           order = c(">5", "<=5")) %>% 
                 visualize(direction = "both", obs_stat = obs_diff)
  )
  
  expect_silent(iris_tbl %>% 
                  specify(Sepal.Width.Group ~ Sepal.Length.Group,
                          success = "large") %>% 
                  hypothesize(null = "independence") %>% 
                  visualize(method = "theoretical")
  )
  
  expect_silent(iris_tbl %>% 
                  specify(Sepal.Width.Group ~ Sepal.Length.Group,
                          success = "large") %>% 
                  hypothesize(null = "independence") %>% 
                  generate(reps = 10, type = "permute") %>% 
                  calculate(stat = "diff in props", 
                            order = c(">5", "<=5")) %>% 
                  visualize(method = "both", direction = "both", obs_stat = obs_diff)
  )
  
  expect_silent(iris_tbl %>% 
                  specify(Sepal.Length ~ Sepal.Width.Group) %>% 
                  hypothesize(null = "independence") %>% 
                  generate(reps = 10, type = "permute") %>% 
                  calculate(stat = "t", order = c("large", "small") ) %>% 
                  visualize(method = "both", direction = "both", 
                            obs_stat = obs_t)
  )
  
  expect_silent(iris_tbl %>% 
                  specify(Sepal.Length ~ Sepal.Length.Group) %>% 
                  hypothesize(null = "independence") %>% 
                  visualize(method = "theoretical")
  )
})