# hypothesize() throws an error when null is not point or independence

    Code
      mtcars_df %>% specify(response = mpg) %>% hypothesize(null = "dependence")
    Condition
      Error:
      ! `null` should be either "point" or "independence".

# hypothesize() throws an error when multiple null values are provided

    Code
      mtcars_df %>% specify(response = mpg) %>% hypothesize(null = c("point",
        "independence"))
    Condition
      Error:
      ! You should specify exactly one type of null hypothesis.

# hypothesize() throws an error when multiple params are set

    Code
      mtcars_df %>% specify(response = mpg) %>% hypothesize(null = "point", mu = 25,
        med = 20)
    Condition
      Error:
      ! You must specify exactly one of `p`, `mu`, `med`, or `sigma`.

# hypothesize() throws an error when p is greater than 1

    Code
      mtcars_df %>% specify(response = vs, success = "1") %>% hypothesize(null = "point",
        p = 1 + .Machine$double.eps)
    Condition
      Error:
      ! `p` should only contain values between zero and one.

# hypothesize() throws an error when p is less than 0

    Code
      mtcars_df %>% specify(response = vs, success = "1") %>% hypothesize(null = "point",
        p = -.Machine$double.neg.eps)
    Condition
      Error:
      ! `p` should only contain values between zero and one.

# hypothesize() throws an error when p contains missing values

    Code
      mtcars_df %>% specify(response = vs, success = "1") %>% hypothesize(null = "point",
        p = c(`0` = 0.5, `1` = NA_real_))
    Condition
      Error:
      ! `p` should not contain missing values.

# hypothesize() throws an error when vector p does not sum to 1

    Code
      mtcars_df %>% specify(response = vs, success = "1") %>% hypothesize(null = "point",
        p = c(`0` = 0.5, `1` = 0.5 + (eps * 2)))
    Condition
      Error:
      ! Make sure the hypothesized values for the `p` parameters sum to 1. Please try again.

# hypothesize arguments function

    Code
      hypothesize(matrix1)
    Condition
      Error in `match_null_hypothesis()`:
      ! argument "null" is missing, with no default

---

    Code
      hypothesize(mtcars_s, null = NA)
    Condition
      Error:
      ! `null` should be either "point" or "independence".

---

    Code
      hypothesize(mtcars_s)
    Condition
      Error in `match_null_hypothesis()`:
      ! argument "null" is missing, with no default

---

    Code
      mtcars_s %>% hypothesize(null = "point", mean = 3)
    Condition
      Error in `hypothesize()`:
      ! unused argument (mean = 3)

---

    Code
      mtcars_s %>% hypothesize(null = "independence")
    Condition
      Error:
      ! Please `specify()` an explanatory and a response variable when testing a null hypothesis of `"independence"`.

---

    Code
      mtcars_s %>% hypothesize(null = "point")
    Condition
      Error:
      ! You must specify exactly one of `p`, `mu`, `med`, or `sigma`.

---

    Code
      mtcars_df %>% dplyr::select(vs) %>% hypothesize(null = "point", mu = 1)
    Condition
      Error in `.subset2()`:
      ! attempt to select less than one element in get1index

---

    Code
      mtcars_df %>% specify(response = vs) %>% hypothesize(null = "point", mu = 1)
    Condition
      Error:
      ! A level of the response variable `vs` needs to be specified for the `success` argument in `specify()`.

---

    Code
      mtcars_s %>% hypothesize(null = "point", p = 0.2)
    Condition
      Error:
      ! A point null regarding a proportion requires that `success` be indicated in `specify()`.

---

    Code
      mtcars_s %>% hypothesize()
    Condition
      Error in `match_null_hypothesis()`:
      ! argument "null" is missing, with no default

# params correct

    Code
      hypothesize(one_prop_specify, null = "point", mu = 2)
    Condition
      Error:
      ! Testing one categorical variable requires `p` to be used as a parameter.

---

    Code
      hypothesize(one_mean_specify, null = "point", mean = 0.5)
    Condition
      Error in `hypothesize()`:
      ! unused argument (mean = 0.5)

