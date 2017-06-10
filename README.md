
-   [Hypothesis tests](#hypothesis-tests)
-   [Confidence intervals](#confidence-intervals)

Infer: a grammar for statistical inference

------------------------------------------------------------------------

The objective of this package is to perform statistical inference using a grammar that illustrates the underlying concepts and a format that coheres with the `tidyverse`. To participate in the discussion surrounding the development of this package, please see the issues.

### Hypothesis tests

![h-test diagram](figs/ht-diagram.png)

#### Examples

These examples assume that `mtcars` has been overwritten so that the variables `cyl`, `vs`, `am`, `gear`, and `carb` are `factor`s:

    mtcars <- as.data.frame(mtcars) %>%
      mutate(cyl = factor(cyl),
             vs = factor(vs),
             am = factor(am),
             gear = factor(gear),
             carb = factor(carb))

One categorical (2 level) variable

    mtcars %>%
      select(am) %>%
      hypothesis(null = "p = 25") %>% # would require string parsing
      generate(reps = 100, type = "permute") %>% # here it’d be doing simulation
      calculate(stat = "prop") #

Two categorical (2 level) variables

    mtcars %>%
      select(am, vs) %>%
      hypothesis(null = "independence") %>% # 
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "diff in prop") #

One categorical (&gt;2 level) - GoF

    mtcars %>%
      select(cyl) %>%
      hypothesis(null = "p1 = .33, p2 = .33, p3 = .33") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "chisq")

Two categorical (&gt;2 level) variables

    mtcars %>%
      select(cyl, am) %>%
      hypothesis(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "chisq")

One numerical variable one categorical (2 levels) (diff in means)

    mtcars %>%
      select(mpg, am) %>%
      hypothesis(null = "equal means") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "diff in means")

One numerical one categorical (&gt;2 levels) - ANOVA

    mtcars %>%
      select(mpg, cyl) %>%
      hypothesis(null = "equal means") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "F")

Two numerical vars - SLR

    mtcars %>%
      select(mpg, hp) %>%
      hypothesis(null = "independence") %>% # or "slope = 0"
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "lm(mpg ~ hp)")

### Confidence intervals

One numerical (one mean)

    mtcars %>%
      select(mpg) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "mean")
