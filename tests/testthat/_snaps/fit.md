# fit.infer messages informatively on excessive null

    Code
      res_ <- fit(hypothesize(specify(gss, hours ~ age + college), null = "independence"))
    Message
      Message: The independence null hypothesis does not inform calculation of the observed fit and will be ignored.

# fit.infer logistic regression works

    Code
      fit(specify(gss, finrela ~ age + college))
    Condition
      Error in `fit()`:
      ! infer does not support fitting models for categorical response variables with more than two levels.
      i Please see `multinom_reg()` from the parsnip package.

