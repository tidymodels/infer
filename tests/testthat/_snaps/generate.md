# cohesion with type argument

    Code
      res_ <- generate(hyp_prop, type = "bootstrap")
    Condition
      Warning:
      You have given `type = "bootstrap"`, but `type` is expected to be `"draw"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(hyp_chisq_gof, type = "bootstrap")
    Condition
      Warning:
      You have given `type = "bootstrap"`, but `type` is expected to be `"draw"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(hyp_diff_in_props, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"permute"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(hyp_chisq_ind, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"permute"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(hyp_mean, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `sample.int()`:
      ! NA in probability vector

---

    Code
      res_ <- generate(hyp_diff_in_means, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"permute"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(hyp_anova, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"permute"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(hyp_prop, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"draw"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `generate()`:
      ! Please `specify()` an explanatory and a response variable when permuting.

---

    Code
      res_ <- generate(hyp_chisq_gof, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"draw"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `generate()`:
      ! Please `specify()` an explanatory and a response variable when permuting.

---

    Code
      res_ <- generate(hyp_mean, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `generate()`:
      ! Please `specify()` an explanatory and a response variable when permuting.

# sensible output

    Code
      generate(hyp_mean, reps = 1, type = "other")
    Condition
      Error in `generate()`:
      ! The `type` argument should be one of "bootstrap", "permute", or "draw". See `generate()` (`?infer::generate()`) for more details.

# auto `type` works (generate)

    Code
      generate(hypothesize(specify(mtcars_df, response = mpg), null = "point", mu = 25),
      reps = 100, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `generate()`:
      ! Please `specify()` an explanatory and a response variable when permuting.

---

    Code
      res_ <- generate(specify(mtcars_df, response = mpg), reps = 100, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(hypothesize(specify(mtcars_df, response = mpg), null = "point",
      med = 26), reps = 100, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `generate()`:
      ! Please `specify()` an explanatory and a response variable when permuting.

---

    Code
      res_ <- generate(hypothesize(specify(mtcars_df, response = am, success = "1"),
      null = "point", p = 0.25), reps = 100, type = "bootstrap")
    Condition
      Warning:
      You have given `type = "bootstrap"`, but `type` is expected to be `"draw"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(hypothesize(specify(mtcars_df, cyl ~ NULL), null = "point", p = c(
        `4` = 0.5, `6` = 0.25, `8` = 0.25)), reps = 100, type = "bootstrap")
    Condition
      Warning:
      You have given `type = "bootstrap"`, but `type` is expected to be `"draw"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(hypothesize(specify(mtcars_df, cyl ~ am), null = "independence"),
      reps = 100, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"permute"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(hypothesize(specify(mtcars_df, mpg ~ cyl), null = "independence"),
      reps = 100, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"permute"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(specify(mtcars_df, response = am, success = "1"), reps = 100,
      type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(specify(mtcars_df, mpg ~ am), reps = 100, type = "permute")
    Condition
      Warning:
      You have given `type = "permute"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `generate()`:
      ! Permuting should be done only when doing an independence hypothesis test. See `hypothesize()` (`?infer::hypothesize()`).

---

    Code
      res_ <- generate(specify(mtcars_df, am ~ vs, success = "1"), reps = 100, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(specify(mtcars_df, mpg ~ hp), reps = 100, type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.

# mismatches lead to error

    Code
      res_ <- generate(mtcars_df, reps = 10, type = "permute")
    Condition
      Error in `generate()`:
      ! The `variables` argument should be one or more unquoted variable names (not strings in quotation marks).

---

    Code
      res_ <- generate(hypothesize(specify(mtcars_df, am ~ NULL, success = "1"),
      null = "independence", p = c(`1` = 0.5)), reps = 100, type = "draw")
    Condition
      Error in `hypothesize()`:
      ! Please `specify()` an explanatory and a response variable when testing a null hypothesis of `"independence"`.

---

    Code
      res_ <- generate(hypothesize(specify(mtcars_df, cyl ~ NULL), null = "point", p = c(
        `4` = 0.5, `6` = 0.25, `8` = 0.25)), reps = 100, type = "bootstrap")
    Condition
      Warning:
      You have given `type = "bootstrap"`, but `type` is expected to be `"draw"`. This workflow is untested and the results may not mean what you think they mean.

---

    Code
      res_ <- generate(specify(mtcars_df, mpg ~ hp), reps = 100, type = "other")
    Condition
      Error in `generate()`:
      ! The `type` argument should be one of "bootstrap", "permute", or "draw". See `generate()` (`?infer::generate()`) for more details.

# generate() handles `NULL` value of `type`

    Code
      res_ <- generate(hyp_prop, type = NULL)
    Message
      Setting `type = "draw"` in `generate()`.

# variables argument prompts when it ought to

    Code
      res_ <- generate(hypothesize(specify(gss[1:10, ], hours ~ age + college), null = "independence"),
      reps = 2, type = "permute", variables = c(howdy))
    Condition
      Error in `generate()`:
      ! The column howdy provided to the `variables` argument is not in the supplied data.

---

    Code
      res <- generate(hypothesize(specify(gss[1:10, ], hours ~ age + college), null = "independence"),
      reps = 2, type = "permute", variables = c(howdy, doo))
    Condition
      Error in `generate()`:
      ! The columns howdy and doo provided to the `variables` argument are not in the supplied data.

---

    Code
      res_ <- generate(hypothesize(specify(gss[1:10, ], hours ~ NULL), null = "point",
      mu = 40), reps = 2, type = "bootstrap", variables = c(hours))
    Condition
      Warning:
      The `variables` argument is only relevant for the "permute" generation type and will be ignored.

---

    Code
      res_ <- generate(hypothesize(specify(gss[1:10, ], hours ~ age + college), null = "independence"),
      reps = 2, type = "permute", variables = "hours")
    Condition
      Error in `generate()`:
      ! The `variables` argument should be one or more unquoted variable names (not strings in quotation marks).

---

    Code
      res_ <- generate(hypothesize(specify(gss[1:10, ], hours ~ age + college + age *
        college), null = "independence"), reps = 2, type = "permute", variables = age *
        college)
    Message
      Message: Please supply only data columns to the `variables` argument. Note that any derived effects that depend on these columns will also be affected.

---

    Code
      res_ <- generate(hypothesize(specify(gss[1:10, ], hours ~ age + college + age *
        college), null = "independence"), reps = 2, type = "permute", variables = c(
        hours, age * college))
    Message
      Message: Please supply only data columns to the `variables` argument. Note that any derived effects that depend on these columns will also be affected.

---

    Code
      res_ <- generate(specify(gss[1:10, ], hours ~ age * college), reps = 2, type = "bootstrap",
      variables = c(hours, age * college))
    Condition
      Warning:
      The `variables` argument is only relevant for the "permute" generation type and will be ignored.

# type = 'draw'/'simulate' superseding handled gracefully

    Code
      res_ <- generate(hypothesize(specify(mtcars_df, response = am, success = "1"),
      null = "point", p = 0.5), type = "simulate")
    Message
      The `"simulate"` generation type has been renamed to `"draw"`. Use `type = "draw"` instead to quiet this message.

---

    Code
      res_ <- generate(hypothesize(specify(mtcars_df, response = am, success = "1"),
      null = "point", p = 0.5), type = "boop")
    Condition
      Error in `generate()`:
      ! The `type` argument should be one of "bootstrap", "permute", or "draw". See `generate()` (`?infer::generate()`) for more details.

---

    Code
      generate(hypothesize(specify(mtcars_df, response = mpg), null = "point", mu = 20),
      type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `sample.int()`:
      ! NA in probability vector

---

    Code
      generate(hypothesize(specify(mtcars_df, response = mpg), null = "point", mu = 20),
      type = "draw")
    Condition
      Warning:
      You have given `type = "draw"`, but `type` is expected to be `"bootstrap"`. This workflow is untested and the results may not mean what you think they mean.
      Error in `sample.int()`:
      ! NA in probability vector

