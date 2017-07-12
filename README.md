
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

------------------------------------------------------------------------

One categorical (2 level) variable

    mtcars %>%
      select(am) %>%
      hypothesize(null = "point", p = c("1" = .25)) %>% 
      generate(reps = 100, type = "simulate") %>% 
      calculate(stat = "prop")

Two categorical (2 level) variables

    mtcars %>%
      select(am, vs) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "diff in props")

One categorical (&gt;2 level) - GoF (not yet implemented)

    mtcars %>%
      select(cyl) %>%
      hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25)) %>% # call levels() to find order of p's
      generate(reps = 100, type = "simulate") %>%
      calculate(stat = "chisq")

Two categorical (&gt;2 level) variables (not yet implemented)

    mtcars %>%
      select(cyl, am) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "chisq")

One numerical variable one categorical (2 levels) (diff in means)

    mtcars %>%
      select(mpg, am) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "diff in means")

One numerical one categorical (&gt;2 levels) - ANOVA (not yet implemented)

    mtcars %>%
      select(mpg, cyl) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "F")

Two numerical vars - SLR (not yet implemented)

    mtcars %>%
      select(mpg, hp) %>%
      hypothesize(null = "independence") %>% # or "slope = 0"
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "lm(mpg ~ hp)")

### Confidence intervals

One numerical (one mean)

    mtcars %>%
      select(mpg) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "mean")
