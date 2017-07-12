
-   [Hypothesis tests](#hypothesis-tests)
-   [Confidence intervals](#confidence-intervals)

Infer: a grammar for statistical inference

------------------------------------------------------------------------

The objective of this package is to perform statistical inference using a grammar that illustrates the underlying concepts and a format that coheres with the `tidyverse`.

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
      specify(response = am) %>% # alt: am ~ 1
      hypothesize(null = "point", p = c("1" = .25)) %>% 
      generate(reps = 100, type = "simulate") %>% 
      calculate(stat = "prop")

Two categorical (2 level) variables

    mtcars %>%
      specify(am ~ vs) %>% # alt: response = am, explanatory = vs
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "diff in props")

One categorical (&gt;2 level) - GoF (not yet implemented)

    mtcars %>%
      specify(response = cyl) %>% # alt: cyl ~ 1
      hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25)) %>%
      generate(reps = 100, type = "simulate") %>%
      calculate(stat = "chisq")

Two categorical (&gt;2 level) variables (not yet implemented)

    mtcars %>%
      specify(cyl ~ am) %>% # alt: response = cyl, explanatory = am
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "chisq")

One numerical variable one categorical (2 levels) (diff in means)

    mtcars %>%
      specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "diff in means")

One numerical one categorical (&gt;2 levels) - ANOVA (not yet implemented)

    mtcars %>%
      specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "F")

Two numerical vars - SLR (not yet implemented)

    mtcars %>%
      specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
      hypothesize(null = "independence") %>% # or "slope = 0"
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "slope")

### Confidence intervals

One numerical (one mean)

    mtcars %>%
      specify(response = mpg) %>% # alt: mpg ~ 1
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "mean")

One categorical (one proportion) (not yet implemented)

    mtcars %>%
      specify(response = am) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "prop")

One numerical variable one categorical (2 levels) (diff in means)

    mtcars %>%
      specify(mpg ~ am) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "diff in means")

Two categorical variables (diff in proportions) (not yet implemented)

    mtcars %>%
      specify(am ~ vs) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "diff in prop")
