context("fit.infer")

x1 <- gss %>% specify(response = hours)
x2 <- gss %>% specify(hours ~ NULL)
x3 <- gss %>% specify(response = hours, explanatory = c(age, college))
x4 <- gss %>% specify(hours ~ age + college)

test_that("get_formula helper works", {
  expect_false(has_attr(x1, "formula"))
  expect_true(has_attr( x2, "formula"))
  expect_false(has_attr(x3, "formula"))
  expect_true(has_attr( x4, "formula"))
  
  expect_equivalent(get_formula(x1), get_formula(x2))
  expect_equivalent(get_formula(x3), get_formula(x4))
})

test_that("fit_linear_model helper works", {
  x3_m <- 
    fit_linear_model(
      x3,
      get_formula(x3),
      "lm"
    )
  
  x4_m <- 
    fit_linear_model(
      x3,
      get_formula(x3),
      "lm"
    )
  
  expect_equal(x3_m, x4_m)
  expect_equal(nrow(x3_m), 3)
  expect_equal(ncol(x3_m), 2)
  
  expect_equal(
    c("term", "estimate"), 
    colnames(x3_m)
  )
  
  expect_equal(
    c("character", "numeric"),
    purrr::map_chr(x3_m, class) %>% unname()
  )
  
  expect_equal(
    c("intercept", "age", "collegedegree"),
    x3_m$term
  )
})

test_that("fit.infer can handle generated objects", {
  x3_fit <- x3 %>% fit()
  
  x3_gen_fit <- x3 %>%
    hypothesize(null = 'independence') %>%
    generate(reps = 2, type = "permute")%>% 
    fit()
  
  expect_equal(unique(x3_fit$term), unique(x3_gen_fit$term))
  expect_equal(nrow(x3_fit) * 2, nrow(x3_gen_fit))
  expect_equal(ncol(x3_fit) + 1, ncol(x3_gen_fit))
  expect_equal(length(unique(x3_gen_fit$replicate)), 2)
  expect_equal(
    colnames(x3_fit),
    colnames(x3_gen_fit)[colnames(x3_gen_fit) != "replicate"]
  )
})
