# cohesion with type argument

    Code
      generate(hyp_mean, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `sample.int()`:
      ! NA in probability vector

---

    Code
      generate(hyp_prop, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"draw"`. This workflow is untested and the results may not mean what you think they mean.
      Error:
      ! Please `specify()` an explanatory and a response variable when permuting.

---

    Code
      generate(hyp_chisq_gof, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"draw"`. This workflow is untested and the results may not mean what you think they mean.
      Error:
      ! Please `specify()` an explanatory and a response variable when permuting.

---

    Code
      generate(hyp_mean, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error:
      ! Please `specify()` an explanatory and a response variable when permuting.

# sensible output

    Code
      generate(hyp_mean, reps = 1, type = "other")
    Condition
      Error:
      ! The `type` argument should be one of "bootstrap", "permute", or "draw". See `?generate` for more details.

# auto `type` works (generate)

    Code
      expect_warning(mtcars_df %>% specify(response = mpg) %>% hypothesize(null = "point",
        mu = 25) %>% generate(reps = 100, type = "permute"))
    Condition
      Error:
      ! Please `specify()` an explanatory and a response variable when permuting.

---

    Code
      mtcars_df %>% specify(response = mpg) %>% hypothesize(null = "point", med = 26) %>%
        generate(reps = 100, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error:
      ! Please `specify()` an explanatory and a response variable when permuting.

---

    Code
      mtcars_df %>% specify(mpg ~ am) %>% generate(reps = 100, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error:
      ! Permuting should be done only when doing independence hypothesis test. See `hypothesize()`.

# mismatches lead to error

    Code
      mtcars_df %>% generate(reps = 10, type = "permute")
    Condition
      Error:
      ! The `variables` argument should be one or more unquoted variable names (not strings in quotation marks).

---

    Code
      mtcars_df %>% specify(am ~ NULL, success = "1") %>% hypothesize(null = "independence",
        p = c(`1` = 0.5)) %>% generate(reps = 100, type = "draw")
    Condition
      Error:
      ! Please `specify()` an explanatory and a response variable when testing a null hypothesis of `"independence"`.

---

    Code
      mtcars_df %>% specify(mpg ~ hp) %>% generate(reps = 100, type = "other")
    Condition
      Error:
      ! The `type` argument should be one of "bootstrap", "permute", or "draw". See `?generate` for more details.

# variables argument prompts when it ought to

    Code
      gss[1:10, ] %>% specify(hours ~ age + college) %>% hypothesize(null = "independence") %>%
        generate(reps = 2, type = "permute", variables = c(howdy))
    Condition
      Error:
      ! The column `list("howdy")` provided to the `variables` argument is not in the supplied data.

---

    Code
      gss[1:10, ] %>% specify(hours ~ age + college) %>% hypothesize(null = "independence") %>%
        generate(reps = 2, type = "permute", variables = c(howdy, doo))
    Condition
      Error:
      ! The columns `list("howdy", "doo")` provided to the `variables` argument are not in the supplied data.

---

    Code
      gss[1:10, ] %>% specify(hours ~ age + college) %>% hypothesize(null = "independence") %>%
        generate(reps = 2, type = "permute", variables = "hours")
    Condition
      Error:
      ! The `variables` argument should be one or more unquoted variable names (not strings in quotation marks).

# type = 'draw'/'simulate' superseding handled gracefully

    Code
      mtcars_df %>% specify(response = am, success = "1") %>% hypothesize(null = "point",
        p = 0.5) %>% generate(type = "boop")
    Condition
      Error:
      ! The `type` argument should be one of "bootstrap", "permute", or "draw". See `?generate` for more details.

---

    Code
      expect_warning(mtcars_df %>% specify(response = mpg) %>% hypothesize(null = "point",
        mu = 20) %>% generate(type = "draw"))
    Condition
      Error in `sample.int()`:
      ! NA in probability vector

---

    Code
      mtcars_df %>% specify(response = mpg) %>% hypothesize(null = "point", mu = 20) %>%
        generate(type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `sample.int()`:
      ! NA in probability vector

