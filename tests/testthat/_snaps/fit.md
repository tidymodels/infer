# fit.infer logistic regression works

    Code
      gss %>% specify(finrela ~ age + college) %>% fit()
    Condition
      Error:
      ! infer does not support fitting models for categorical response variables with more than two levels. Please see `multinom_reg()` from the parsnip package.

