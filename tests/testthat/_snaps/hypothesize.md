# hypothesize() throws an error when null is not point or independence

    Code
      mtcars_df %>% specify(response = mpg) %>% hypothesize(null = "dependence")
    Condition
      Error in `hypothesize()`:
      ! `null` should be either "point", "independence", or "paired independence".

# hypothesize() throws an error when multiple null values are provided

    Code
      mtcars_df %>% specify(response = mpg) %>% hypothesize(null = c("point",
        "independence"))
    Condition
      Error in `hypothesize()`:
      ! You should specify exactly one type of null hypothesis.

# hypothesize() throws an error when multiple params are set

    Code
      mtcars_df %>% specify(response = mpg) %>% hypothesize(null = "point", mu = 25,
        med = 20)
    Condition
      Error in `hypothesize()`:
      ! You must specify exactly one of `p`, `mu`, `med`, or `sigma`.

# hypothesize() throws a warning when params are set with independence

    Code
      res_ <- mtcars_df %>% specify(mpg ~ vs) %>% hypothesize(null = "independence",
        mu = 25)
    Condition
      Warning:
      Parameter values are not specified when testing that two variables are independent.

# hypothesize() throws a warning when params are set with paired independence

    Code
      res_ <- mtcars_df %>% specify(response = mpg) %>% hypothesize(null = "paired independence",
        mu = 25)
    Condition
      Warning:
      Parameter values are not specified when testing paired independence.

# hypothesize() throws an error when p is greater than 1

    Code
      res_ <- mtcars_df %>% specify(response = vs, success = "1") %>% hypothesize(
        null = "point", p = 1 + .Machine$double.eps)
    Condition
      Error in `hypothesize()`:
      ! `p` should only contain values between zero and one.

# hypothesize() throws an error when p is less than 0

    Code
      res_ <- mtcars_df %>% specify(response = vs, success = "1") %>% hypothesize(
        null = "point", p = -.Machine$double.neg.eps)
    Condition
      Error in `hypothesize()`:
      ! `p` should only contain values between zero and one.

# hypothesize() throws an error when p contains missing values

    Code
      res_ <- mtcars_df %>% specify(response = vs, success = "1") %>% hypothesize(
        null = "point", p = c(`0` = 0.5, `1` = NA_real_))
    Condition
      Error in `hypothesize()`:
      ! `p` should not contain missing values.

# hypothesize() throws an error when vector p does not sum to 1

    Code
      res_ <- mtcars_df %>% specify(response = vs, success = "1") %>% hypothesize(
        null = "point", p = c(`0` = 0.5, `1` = 0.5 + (eps * 2)))
    Condition
      Error in `hypothesize()`:
      ! Make sure the hypothesized values for the `p` parameters sum to 1. Please try again.

# hypothesize arguments function

    Code
      res_ <- hypothesize(matrix1)
    Condition
      Error in `hypothesize()`:
      ! `null` should be either "point", "independence", or "paired independence".

---

    Code
      res_ <- hypothesize(mtcars_s, null = NA)
    Condition
      Error in `hypothesize()`:
      ! `null` should be either "point", "independence", or "paired independence".

---

    Code
      res_ <- hypothesize(mtcars_s)
    Condition
      Error in `hypothesize()`:
      ! `null` should be either "point", "independence", or "paired independence".

---

    Code
      res_ <- mtcars_s %>% hypothesize(null = "point", mean = 3)
    Condition
      Error in `hypothesize()`:
      ! unused argument (mean = 3)

---

    Code
      res_ <- mtcars_s %>% hypothesize(null = "independence")
    Condition
      Error in `hypothesize()`:
      ! Please `specify()` an explanatory and a response variable when testing a null hypothesis of `"independence"`.

---

    Code
      res_ <- mtcars_s %>% hypothesize(null = "point")
    Condition
      Error in `hypothesize()`:
      ! You must specify exactly one of `p`, `mu`, `med`, or `sigma`.

---

    Code
      res_ <- mtcars_f %>% specify(mpg ~ am) %>% hypothesize(null = "paired independence")
    Condition
      Error in `hypothesize()`:
      ! Please `specify()` only a response variable when testing a null hypothesis of `"paired independence"`.
      i The supplied response variable should be the pre-computed difference between paired observations.

---

    Code
      res <- mtcars_s %>% hypothesize(null = c("point", "independence"), mu = 3)
    Condition
      Error in `hypothesize()`:
      ! You should specify exactly one type of null hypothesis.

---

    Code
      res_ <- mtcars_df %>% dplyr::select(vs) %>% hypothesize(null = "point", mu = 1)
    Condition
      Error in `.subset2()`:
      ! attempt to select less than one element in get1index

---

    Code
      res_ <- mtcars_df %>% specify(response = vs) %>% hypothesize(null = "point",
        mu = 1)
    Condition
      Error in `specify()`:
      ! A level of the response variable `vs` needs to be specified for the `success` argument in `specify()`.

---

    Code
      res_ <- mtcars_s %>% hypothesize(null = "point", p = 0.2)
    Condition
      Error in `hypothesize()`:
      ! A point null regarding a proportion requires that `success` be indicated in `specify()`.

---

    Code
      res_ <- mtcars_s %>% hypothesize()
    Condition
      Error in `hypothesize()`:
      ! `null` should be either "point", "independence", or "paired independence".

# params correct

    Code
      res_ <- hypothesize(one_prop_specify, null = "point", mu = 2)
    Condition
      Error in `hypothesize()`:
      ! Testing one categorical variable requires `p` to be used as a parameter.

---

    Code
      res_ <- hypothesize(one_mean_specify, null = "point", mean = 0.5)
    Condition
      Error in `hypothesize()`:
      ! unused argument (mean = 0.5)

# user can specify multiple explanatory variables

    Code
      res_ <- gss %>% specify(hours ~ sex + college) %>% hypothesize(null = "independence",
        mu = 40)
    Condition
      Warning:
      Parameter values are not specified when testing that two variables are independent.

