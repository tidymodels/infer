# fit.infer messages informatively on excessive null

    Code
      res_ <- gss %>% specify(hours ~ age + college) %>% hypothesize(null = "independence") %>%
        fit()
    Message
      Message: The independence null hypothesis does not inform calculation of the observed fit and will be ignored.

# fit.infer logistic regression works

    Code
      gss %>% specify(finrela ~ age + college) %>% fit()
    Condition
      Error in `fit()`:
      ! infer does not support fitting models for categorical response variables with more than two levels.
      i Please see `multinom_reg()` from the parsnip package.

