# fit.infer messages informatively on excessive null

    Code
      gss %>% specify(hours ~ age + college) %>% hypothesize(null = "independence") %>%
        fit()
    Message
      Message: The independence null hypothesis does not inform calculation of the observed fit and will be ignored.
    Output
      # A tibble: 3 x 2
        term          estimate
        <chr>            <dbl>
      1 intercept     40.6    
      2 age            0.00596
      3 collegedegree  1.53   

# fit.infer logistic regression works

    Code
      gss %>% specify(finrela ~ age + college) %>% fit()
    Condition
      Error:
      ! infer does not support fitting models for categorical response variables with more than two levels. Please see `multinom_reg()` from the parsnip package.

