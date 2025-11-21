# Full infer Pipeline Examples

#### Introduction

This vignette is intended to provide a set of examples that nearly
exhaustively demonstrate the functionalities provided by infer.
Commentary on these examples is limited—for more discussion of the
intuition behind the package, see the “Getting to Know infer” vignette,
accessible by calling
[`vignette("infer")`](https://infer.tidymodels.org/dev/articles/infer.md).

Throughout this vignette, we’ll make use of the `gss` dataset supplied
by infer, which contains a sample of data from the General Social
Survey. See [`?gss`](https://infer.tidymodels.org/dev/reference/gss.md)
for more information on the variables included and their source. Note
that this data (and our examples on it) are for demonstration purposes
only, and will not necessarily provide accurate estimates unless
weighted properly. For these examples, let’s suppose that this dataset
is a representative sample of a population we want to learn about:
American adults. The data looks like this:

``` r
# load in the dataset
data(gss)

# take a look at its structure
dplyr::glimpse(gss)
```

    ## Rows: 500
    ## Columns: 11
    ## $ year    <dbl> 2014, 1994, 1998, 1996, 1994, 1996, 1990, 2016, 2000,…
    ## $ age     <dbl> 36, 34, 24, 42, 31, 32, 48, 36, 30, 33, 21, 30, 38, 4…
    ## $ sex     <fct> male, female, male, male, male, female, female, femal…
    ## $ college <fct> degree, no degree, degree, no degree, degree, no degr…
    ## $ partyid <fct> ind, rep, ind, ind, rep, rep, dem, ind, rep, dem, dem…
    ## $ hompop  <dbl> 3, 4, 1, 4, 2, 4, 2, 1, 5, 2, 4, 3, 4, 4, 2, 2, 3, 2,…
    ## $ hours   <dbl> 50, 31, 40, 40, 40, 53, 32, 20, 40, 40, 23, 52, 38, 7…
    ## $ income  <ord> $25000 or more, $20000 - 24999, $25000 or more, $2500…
    ## $ class   <fct> middle class, working class, working class, working c…
    ## $ finrela <fct> below average, below average, below average, above av…
    ## $ weight  <dbl> 0.8960, 1.0825, 0.5501, 1.0864, 1.0825, 1.0864, 1.062…

## Hypothesis tests

### One numerical variable (mean)

Calculating the observed statistic,

``` r
x_bar <- gss |>
  specify(response = hours) |>
  calculate(stat = "mean")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
x_bar <- gss |>
  observe(response = hours, stat = "mean")
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40) |>
  generate(reps = 1000) |>
  calculate(stat = "mean")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = x_bar, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-5-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = x_bar, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.022

### One numerical variable (standardized mean $t$)

Calculating the observed statistic,

``` r
t_bar <- gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40) |>
  calculate(stat = "t")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
t_bar <- gss |>
  observe(response = hours, null = "point", mu = 40, stat = "t")
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40) |>
  generate(reps = 1000) |>
  calculate(stat = "t")
```

Alternatively, finding the null distribution using theoretical methods
using the
[`assume()`](https://infer.tidymodels.org/dev/reference/assume.md) verb,

``` r
null_dist_theory <- gss |>
  specify(response = hours)  |>
  assume("t")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = t_bar, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-11-1.png)

Alternatively, visualizing the observed statistic using the theory-based
null distribution,

``` r
visualize(null_dist_theory) +
  shade_p_value(obs_stat = t_bar, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-12-1.png)

Alternatively, visualizing the observed statistic using both of the null
distributions,

``` r
visualize(null_dist, method = "both") +
  shade_p_value(obs_stat = t_bar, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-13-1.png)

Note that the above code makes use of the randomization-based null
distribution.

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = t_bar, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.028

Alternatively, using the
[`t_test()`](https://infer.tidymodels.org/dev/reference/t_test.md)
wrapper:

``` r
gss |>
  t_test(response = hours, mu = 40)
```

    ## # A tibble: 1 × 7
    ##   statistic  t_df p_value alternative estimate lower_ci upper_ci
    ##       <dbl> <dbl>   <dbl> <chr>          <dbl>    <dbl>    <dbl>
    ## 1      2.09   499  0.0376 two.sided       41.4     40.1     42.7

`infer` does not support testing on one numerical variable via the `z`
distribution.

### One numerical variable (median)

Calculating the observed statistic,

``` r
x_tilde <- gss |>
  specify(response = age) |>
  calculate(stat = "median")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
x_tilde <- gss |>
  observe(response = age, stat = "median")
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(response = age) |>
  hypothesize(null = "point", med = 40) |> 
  generate(reps = 1000) |> 
  calculate(stat = "median")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = x_tilde, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-19-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = x_tilde, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.008

### One numerical variable (paired)

The example under this header is compatible with `stat`s `"mean"`,
`"median"`, `"sum"`, and `"sd"`.

Suppose that each of these survey respondents had provided the number of
`hours` worked per week when surveyed 5 years prior, encoded as
`hours_previous`.

``` r
set.seed(1)

gss_paired <- gss |>
   mutate(
      hours_previous = hours + 5 - rpois(nrow(gss), 4.8),
      diff = hours - hours_previous
   )

gss_paired |>
   select(hours, hours_previous, diff)
```

    ## # A tibble: 500 × 3
    ##    hours hours_previous  diff
    ##    <dbl>          <dbl> <dbl>
    ##  1    50             52    -2
    ##  2    31             32    -1
    ##  3    40             40     0
    ##  4    40             37     3
    ##  5    40             42    -2
    ##  6    53             50     3
    ##  7    32             28     4
    ##  8    20             19     1
    ##  9    40             40     0
    ## 10    40             43    -3
    ## # ℹ 490 more rows

We’d like to test the null hypothesis that the `"mean"` hours worked per
week did not change between the sampled time and five years prior.

infer supports paired hypothesis testing via the
`null = "paired independence"` argument to
[`hypothesize()`](https://infer.tidymodels.org/dev/reference/hypothesize.md).

Calculating the observed statistic,

``` r
x_tilde <- gss_paired |>
  specify(response = diff) |>
  calculate(stat = "mean")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
x_tilde <- gss_paired |>
  observe(response = diff, stat = "mean")
```

Then, generating the null distribution,

``` r
null_dist <- gss_paired |>
  specify(response = diff) |>
  hypothesize(null = "paired independence") |> 
  generate(reps = 1000, type = "permute") |> 
  calculate(stat = "mean")
```

Note that the `diff` column itself is not permuted, but rather the signs
of the values in the column.

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = x_tilde, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-25-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = x_tilde, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.028

### One categorical (one proportion)

Calculating the observed statistic,

``` r
p_hat <- gss |>
  specify(response = sex, success = "female") |>
  calculate(stat = "prop")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
p_hat <- gss |>
  observe(response = sex, success = "female", stat = "prop")
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(response = sex, success = "female") |>
  hypothesize(null = "point", p = .5) |>
  generate(reps = 1000) |>
  calculate(stat = "prop")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = p_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-30-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = p_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.276

Note that logical variables will be coerced to factors:

``` r
null_dist <- gss |>
  dplyr::mutate(is_female = (sex == "female")) |>
  specify(response = is_female, success = "TRUE") |>
  hypothesize(null = "point", p = .5) |>
  generate(reps = 1000) |>
  calculate(stat = "prop")
```

### One categorical variable (standardized proportion $z$)

Calculating the observed statistic,

``` r
p_hat <- gss |>
  specify(response = sex, success = "female") |>
  hypothesize(null = "point", p = .5) |>
  calculate(stat = "z")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
p_hat <- gss |>
  observe(response = sex, success = "female", null = "point", p = .5, stat = "z")
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(response = sex, success = "female") |>
  hypothesize(null = "point", p = .5) |>
  generate(reps = 1000, type = "draw") |>
  calculate(stat = "z")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = p_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-36-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = p_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.252

The package also supplies a wrapper around
[`prop.test()`](https://rdrr.io/r/stats/prop.test.html) for tests of a
single proportion on tidy data.

``` r
prop_test(gss,
          college ~ NULL,
          p = .2)
```

    ## # A tibble: 1 × 4
    ##   statistic chisq_df   p_value alternative
    ##       <dbl>    <int>     <dbl> <chr>      
    ## 1      636.        1 2.98e-140 two.sided

infer does not support testing two means via the `z` distribution.

### Two categorical (2 level) variables

The `infer` package provides several statistics to work with data of
this type. One of them is the statistic for difference in proportions.

Calculating the observed statistic,

``` r
d_hat <- gss |> 
  specify(college ~ sex, success = "no degree") |>
  calculate(stat = "diff in props", order = c("female", "male"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
d_hat <- gss |>
  observe(
    college ~ sex,
    success = "no degree",
    stat = "diff in props", order = c("female", "male")
  )
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(college ~ sex, success = "no degree") |>
  hypothesize(null = "independence") |>
  generate(reps = 1000) |>
  calculate(stat = "diff in props", order = c("female", "male"))
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = d_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-41-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = d_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1       1

infer also provides functionality to calculate ratios of proportions.
The workflow looks similar to that for `diff in props`.

Calculating the observed statistic,

``` r
r_hat <- gss |> 
  specify(college ~ sex, success = "no degree") |>
  calculate(stat = "ratio of props", order = c("female", "male"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
r_hat <- gss |> 
  observe(college ~ sex, success = "no degree",
          stat = "ratio of props", order = c("female", "male"))
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(college ~ sex, success = "no degree") |>
  hypothesize(null = "independence") |> 
  generate(reps = 1000) |> 
  calculate(stat = "ratio of props", order = c("female", "male"))
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = r_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-46-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = r_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1       1

In addition, the package provides functionality to calculate odds
ratios. The workflow also looks similar to that for `diff in props`.

Calculating the observed statistic,

``` r
or_hat <- gss |> 
  specify(college ~ sex, success = "no degree") |>
  calculate(stat = "odds ratio", order = c("female", "male"))
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(college ~ sex, success = "no degree") |>
  hypothesize(null = "independence") |> 
  generate(reps = 1000) |> 
  calculate(stat = "odds ratio", order = c("female", "male"))
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = or_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-50-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = or_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.984

### Two categorical (2 level) variables (z)

Finding the standardized observed statistic,

``` r
z_hat <- gss |> 
  specify(college ~ sex, success = "no degree") |>
  hypothesize(null = "independence") |>
  calculate(stat = "z", order = c("female", "male"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
z_hat <- gss |> 
  observe(college ~ sex, success = "no degree",
          stat = "z", order = c("female", "male"))
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(college ~ sex, success = "no degree") |>
  hypothesize(null = "independence") |> 
  generate(reps = 1000) |> 
  calculate(stat = "z", order = c("female", "male"))
```

Alternatively, finding the null distribution using theoretical methods
using the
[`assume()`](https://infer.tidymodels.org/dev/reference/assume.md) verb,

``` r
null_dist_theory <- gss |>
  specify(college ~ sex, success = "no degree") |>
  assume("z")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = z_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-56-1.png)

Alternatively, visualizing the observed statistic using the theory-based
null distribution,

``` r
visualize(null_dist_theory) +
  shade_p_value(obs_stat = z_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-57-1.png)

Alternatively, visualizing the observed statistic using both of the null
distributions,

``` r
visualize(null_dist, method = "both") +
  shade_p_value(obs_stat = z_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-58-1.png)

Note that the above code makes use of the randomization-based null
distribution.

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = z_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1    0.98

Note the similarities in this plot and the previous one.

The package also supplies a wrapper around `prop.test` to allow for
tests of equality of proportions on tidy data.

``` r
prop_test(gss, 
          college ~ sex,  
          order = c("female", "male"))
```

    ## # A tibble: 1 × 6
    ##   statistic chisq_df p_value alternative lower_ci upper_ci
    ##       <dbl>    <dbl>   <dbl> <chr>          <dbl>    <dbl>
    ## 1 0.0000204        1   0.996 two.sided    -0.0918   0.0834

### One categorical (\>2 level) - GoF

Calculating the observed statistic,

Note the need to add in the hypothesized values here to compute the
observed statistic.

``` r
Chisq_hat <- gss |>
  specify(response = finrela) |>
  hypothesize(
    null = "point",
    p = c(
      "far below average" = 1 / 6,
      "below average" = 1 / 6,
      "average" = 1 / 6,
      "above average" = 1 / 6,
      "far above average" = 1 / 6,
      "DK" = 1 / 6
    )
  ) |>
  calculate(stat = "Chisq")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
Chisq_hat <- gss |>
  observe(
    response = finrela,
    null = "point",
    p = c(
      "far below average" = 1 / 6,
      "below average" = 1 / 6,
      "average" = 1 / 6,
      "above average" = 1 / 6,
      "far above average" = 1 / 6,
      "DK" = 1 / 6
    ),
    stat = "Chisq"
  )
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(response = finrela) |>
  hypothesize(
    null = "point",
    p = c(
      "far below average" = 1 / 6,
      "below average" = 1 / 6,
      "average" = 1 / 6,
      "above average" = 1 / 6,
      "far above average" = 1 / 6,
      "DK" = 1 / 6
    )
  ) |>
  generate(reps = 1000, type = "draw") |>
  calculate(stat = "Chisq")
```

Alternatively, finding the null distribution using theoretical methods
using the
[`assume()`](https://infer.tidymodels.org/dev/reference/assume.md) verb,

``` r
null_dist_theory <- gss |>
  specify(response = finrela) |>
  assume("Chisq")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-64-1.png)

Alternatively, visualizing the observed statistic using the theory-based
null distribution,

``` r
visualize(null_dist_theory) +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-65-1.png)

Alternatively, visualizing the observed statistic using both of the null
distributions,

``` r
visualize(null_dist_theory, method = "both") +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-66-1.png)

Note that the above code makes use of the randomization-based null
distribution.

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = Chisq_hat, direction = "greater")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1       0

Alternatively, using the `chisq_test` wrapper:

``` r
chisq_test(
  gss,
  response = finrela,
  p = c(
    "far below average" = 1 / 6,
    "below average" = 1 / 6,
    "average" = 1 / 6,
    "above average" = 1 / 6,
    "far above average" = 1 / 6,
    "DK" = 1 / 6
  )
)
```

    ## # A tibble: 1 × 3
    ##   statistic chisq_df   p_value
    ##       <dbl>    <dbl>     <dbl>
    ## 1      488.        5 3.13e-103

### Two categorical (\>2 level): Chi-squared test of independence

Calculating the observed statistic,

``` r
Chisq_hat <- gss |>
  specify(formula = finrela ~ sex) |> 
  hypothesize(null = "independence") |>
  calculate(stat = "Chisq")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
Chisq_hat <- gss |>
  observe(formula = finrela ~ sex, stat = "Chisq")
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(finrela ~ sex) |>
  hypothesize(null = "independence") |> 
  generate(reps = 1000, type = "permute") |> 
  calculate(stat = "Chisq")
```

Alternatively, finding the null distribution using theoretical methods
using the
[`assume()`](https://infer.tidymodels.org/dev/reference/assume.md) verb,

``` r
null_dist_theory <- gss |>
  specify(finrela ~ sex) |>
  assume(distribution = "Chisq")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-73-1.png)

Alternatively, visualizing the observed statistic using the theory-based
null distribution,

``` r
visualize(null_dist_theory) +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-74-1.png)

Alternatively, visualizing the observed statistic using both of the null
distributions,

``` r
visualize(null_dist, method = "both") +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-75-1.png)

Note that the above code makes use of the randomization-based null
distribution.

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = Chisq_hat, direction = "greater")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.118

Alternatively, using the wrapper to carry out the test,

``` r
gss |>
  chisq_test(formula = finrela ~ sex)
```

    ## # A tibble: 1 × 3
    ##   statistic chisq_df p_value
    ##       <dbl>    <int>   <dbl>
    ## 1      9.11        5   0.105

### One numerical variable, one categorical (2 levels) (diff in means)

Calculating the observed statistic,

``` r
d_hat <- gss |> 
  specify(age ~ college) |> 
  calculate(stat = "diff in means", order = c("degree", "no degree"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
d_hat <- gss |> 
  observe(age ~ college,
          stat = "diff in means", order = c("degree", "no degree"))
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(age ~ college) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "diff in means", order = c("degree", "no degree"))
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = d_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-81-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = d_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1    0.46

### One numerical variable, one categorical (2 levels) (t)

Finding the standardized observed statistic,

``` r
t_hat <- gss |> 
  specify(age ~ college) |> 
  hypothesize(null = "independence") |>
  calculate(stat = "t", order = c("degree", "no degree"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
t_hat <- gss |> 
  observe(age ~ college,
          stat = "t", order = c("degree", "no degree"))
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(age ~ college) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "t", order = c("degree", "no degree"))
```

Alternatively, finding the null distribution using theoretical methods
using the
[`assume()`](https://infer.tidymodels.org/dev/reference/assume.md) verb,

``` r
null_dist_theory <- gss |>
  specify(age ~ college) |>
  assume("t")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = t_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-87-1.png)

Alternatively, visualizing the observed statistic using the theory-based
null distribution,

``` r
visualize(null_dist_theory) +
  shade_p_value(obs_stat = t_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-88-1.png)

Alternatively, visualizing the observed statistic using both of the null
distributions,

``` r
visualize(null_dist, method = "both") +
  shade_p_value(obs_stat = t_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-89-1.png)

Note that the above code makes use of the randomization-based null
distribution.

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = t_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.442

Note the similarities in this plot and the previous one.

### One numerical variable, one categorical (2 levels) (diff in medians)

Calculating the observed statistic,

``` r
d_hat <- gss |> 
  specify(age ~ college) |> 
  calculate(stat = "diff in medians", order = c("degree", "no degree"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
d_hat <- gss |> 
  observe(age ~ college,
          stat = "diff in medians", order = c("degree", "no degree"))
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
  specify(age ~ college) |> # alt: response = age, explanatory = season
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "diff in medians", order = c("degree", "no degree"))
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = d_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-94-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = d_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.172

### One numerical, one categorical (\>2 levels) - ANOVA

Calculating the observed statistic,

``` r
F_hat <- gss |> 
  specify(age ~ partyid) |>
  calculate(stat = "F")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
F_hat <- gss |> 
  observe(age ~ partyid, stat = "F")
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
   specify(age ~ partyid) |>
   hypothesize(null = "independence") |>
   generate(reps = 1000, type = "permute") |>
   calculate(stat = "F")
```

Alternatively, finding the null distribution using theoretical methods
using the
[`assume()`](https://infer.tidymodels.org/dev/reference/assume.md) verb,

``` r
null_dist_theory <- gss |>
   specify(age ~ partyid) |>
   hypothesize(null = "independence") |>
   assume(distribution = "F")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = F_hat, direction = "greater")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-100-1.png)

Alternatively, visualizing the observed statistic using the theory-based
null distribution,

``` r
visualize(null_dist_theory) +
  shade_p_value(obs_stat = F_hat, direction = "greater")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-101-1.png)

Alternatively, visualizing the observed statistic using both of the null
distributions,

``` r
visualize(null_dist, method = "both") +
  shade_p_value(obs_stat = F_hat, direction = "greater")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-102-1.png)

Note that the above code makes use of the randomization-based null
distribution.

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = F_hat, direction = "greater")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.045

### Two numerical vars - SLR

Calculating the observed statistic,

``` r
slope_hat <- gss |> 
  specify(hours ~ age) |> 
  calculate(stat = "slope")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
slope_hat <- gss |> 
  observe(hours ~ age, stat = "slope")
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
   specify(hours ~ age) |> 
   hypothesize(null = "independence") |>
   generate(reps = 1000, type = "permute") |>
   calculate(stat = "slope")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = slope_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-107-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = slope_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.902

### Two numerical vars - correlation

Calculating the observed statistic,

``` r
correlation_hat <- gss |> 
  specify(hours ~ age) |> 
  calculate(stat = "correlation")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
correlation_hat <- gss |> 
  observe(hours ~ age, stat = "correlation")
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
   specify(hours ~ age) |> 
   hypothesize(null = "independence") |>
   generate(reps = 1000, type = "permute") |>
   calculate(stat = "correlation")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = correlation_hat, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-112-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = correlation_hat, direction = "two-sided")
```

    ## # A tibble: 1 × 1
    ##   p_value
    ##     <dbl>
    ## 1   0.878

### Two numerical vars - SLR (t)

Not currently implemented since $t$ could refer to standardized slope or
standardized correlation.

### Multiple explanatory variables

Calculating the observed fit,

``` r
obs_fit <- gss |>
  specify(hours ~ age + college) |>
  fit()
```

Generating a distribution of fits with the response variable permuted,

``` r
null_dist <- gss |>
  specify(hours ~ age + college) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  fit()
```

Generating a distribution of fits where each explanatory variable is
permuted independently,

``` r
null_dist2 <- gss |>
  specify(hours ~ age + college) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute", variables = c(age, college)) |>
  fit()
```

Visualizing the observed fit alongside the null fits,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = obs_fit, direction = "two-sided")
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-117-1.png)

Calculating p-values from the null distribution and observed fit,

``` r
null_dist |>
  get_p_value(obs_stat = obs_fit, direction = "two-sided")
```

    ## # A tibble: 3 × 2
    ##   term          p_value
    ##   <chr>           <dbl>
    ## 1 age             0.914
    ## 2 collegedegree   0.266
    ## 3 intercept       0.734

Note that this
[`fit()`](https://generics.r-lib.org/reference/fit.html)-based workflow
can be applied to use cases with differing numbers of explanatory
variables and explanatory variable types.

## Confidence intervals

### One numerical (one mean)

Finding the observed statistic,

``` r
x_bar <- gss |> 
  specify(response = hours) |>
  calculate(stat = "mean")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
x_bar <- gss |> 
  observe(response = hours, stat = "mean")
```

Then, generating a bootstrap distribution,

``` r
boot_dist <- gss |>
   specify(response = hours) |>
   generate(reps = 1000, type = "bootstrap") |>
   calculate(stat = "mean")
```

Use the bootstrap distribution to find a confidence interval,

``` r
percentile_ci <- get_ci(boot_dist)
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-123-1.png)

Alternatively, use the bootstrap distribution to find a confidence
interval using the standard error,

``` r
standard_error_ci <- get_ci(boot_dist, type = "se", point_estimate = x_bar)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-124-1.png)

Instead of a simulation-based bootstrap distribution, we can also define
a theory-based sampling distribution,

``` r
sampling_dist <- gss |>
   specify(response = hours) |>
   assume(distribution = "t")
```

Visualization and calculation of confidence intervals interfaces in the
same way as with the simulation-based distribution,

``` r
theor_ci <- get_ci(sampling_dist, point_estimate = x_bar)

theor_ci
```

    ## # A tibble: 1 × 2
    ##   lower_ci upper_ci
    ##      <dbl>    <dbl>
    ## 1     40.1     42.7

``` r
visualize(sampling_dist) +
  shade_confidence_interval(endpoints = theor_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-126-1.png)

Note that the `t` distribution is recentered and rescaled to lie on the
scale of the observed data. infer does not support confidence intervals
on means via the `z` distribution.

### One numerical (one mean - standardized)

Finding the observed statistic,

``` r
t_hat <- gss |> 
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40) |>
  calculate(stat = "t")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
t_hat <- gss |> 
  observe(response = hours,
          null = "point", mu = 40,
          stat = "t")
```

Then, generating the bootstrap distribution,

``` r
boot_dist <- gss |>
   specify(response = hours) |>
   generate(reps = 1000, type = "bootstrap") |>
   calculate(stat = "t")
```

Use the bootstrap distribution to find a confidence interval,

``` r
percentile_ci <- get_ci(boot_dist)
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-131-1.png)

Alternatively, use the bootstrap distribution to find a confidence
interval using the standard error,

``` r
standard_error_ci <- boot_dist |>
  get_ci(type = "se", point_estimate = t_hat)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-132-1.png)

See the above subsection (one mean) for a theory-based approach. Note
that infer does not support confidence intervals on means via the `z`
distribution.

### One categorical (one proportion)

Finding the observed statistic,

``` r
p_hat <- gss |> 
   specify(response = sex, success = "female") |>
   calculate(stat = "prop")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
p_hat <- gss |> 
   observe(response = sex, success = "female", stat = "prop")
```

Then, generating a bootstrap distribution,

``` r
boot_dist <- gss |>
 specify(response = sex, success = "female") |>
 generate(reps = 1000, type = "bootstrap") |>
 calculate(stat = "prop")
```

Use the bootstrap distribution to find a confidence interval,

``` r
percentile_ci <- get_ci(boot_dist)
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-137-1.png)

Alternatively, use the bootstrap distribution to find a confidence
interval using the standard error,

``` r
standard_error_ci <- boot_dist |>
  get_ci(type = "se", point_estimate = p_hat)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-138-1.png)

Instead of a simulation-based bootstrap distribution, we can also define
a theory-based sampling distribution,

``` r
sampling_dist <- gss |>
   specify(response = sex, success = "female") |>
   assume(distribution = "z")
```

Visualization and calculation of confidence intervals interfaces in the
same way as with the simulation-based distribution,

``` r
theor_ci <- get_ci(sampling_dist, point_estimate = p_hat)

theor_ci
```

    ## # A tibble: 1 × 2
    ##   lower_ci upper_ci
    ##      <dbl>    <dbl>
    ## 1    0.430    0.518

``` r
visualize(sampling_dist) +
  shade_confidence_interval(endpoints = theor_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-140-1.png)

Note that the `z` distribution is recentered and rescaled to lie on the
scale of the observed data. `infer` does not support confidence
intervals on means via the `z` distribution.

### One categorical variable (standardized proportion $z$)

See the above subsection (one proportion) for a theory-based approach.

### One numerical variable, one categorical (2 levels) (diff in means)

Finding the observed statistic,

``` r
d_hat <- gss |>
  specify(hours ~ college) |>
  calculate(stat = "diff in means", order = c("degree", "no degree"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
d_hat <- gss |>
  observe(hours ~ college,
          stat = "diff in means", order = c("degree", "no degree"))
```

Then, generating a bootstrap distribution,

``` r
boot_dist <- gss |>
   specify(hours ~ college) |>
   generate(reps = 1000, type = "bootstrap") |>
   calculate(stat = "diff in means", order = c("degree", "no degree"))
```

Use the bootstrap distribution to find a confidence interval,

``` r
percentile_ci <- get_ci(boot_dist)
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-145-1.png)

Alternatively, use the bootstrap distribution to find a confidence
interval using the standard error,

``` r
standard_error_ci <- boot_dist |>
  get_ci(type = "se", point_estimate = d_hat)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-146-1.png)

Instead of a simulation-based bootstrap distribution, we can also define
a theory-based sampling distribution,

``` r
sampling_dist <- gss |>
   specify(hours ~ college) |>
   assume(distribution = "t")
```

Visualization and calculation of confidence intervals interfaces in the
same way as with the simulation-based distribution,

``` r
theor_ci <- get_ci(sampling_dist, point_estimate = d_hat)

theor_ci
```

    ## # A tibble: 1 × 2
    ##   lower_ci upper_ci
    ##      <dbl>    <dbl>
    ## 1    -1.16     4.24

``` r
visualize(sampling_dist) +
  shade_confidence_interval(endpoints = theor_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-148-1.png)

Note that the `t` distribution is recentered and rescaled to lie on the
scale of the observed data.

`infer` also provides functionality to calculate ratios of means. The
workflow looks similar to that for `diff in means`.

Finding the observed statistic,

``` r
d_hat <- gss |>
  specify(hours ~ college) |>
  calculate(stat = "ratio of means", order = c("degree", "no degree"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
d_hat <- gss |>
  observe(hours ~ college,
          stat = "ratio of means", order = c("degree", "no degree"))
```

Then, generating a bootstrap distribution,

``` r
boot_dist <- gss |>
   specify(hours ~ college) |>
   generate(reps = 1000, type = "bootstrap") |>
   calculate(stat = "ratio of means", order = c("degree", "no degree"))
```

Use the bootstrap distribution to find a confidence interval,

``` r
percentile_ci <- get_ci(boot_dist)
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-153-1.png)

Alternatively, use the bootstrap distribution to find a confidence
interval using the standard error,

``` r
standard_error_ci <- boot_dist |>
  get_ci(type = "se", point_estimate = d_hat)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-154-1.png)

### One numerical variable, one categorical (2 levels) (t)

Finding the standardized point estimate,

``` r
t_hat <- gss |>
  specify(hours ~ college) |>
  calculate(stat = "t", order = c("degree", "no degree"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
t_hat <- gss |>
  observe(hours ~ college,
          stat = "t", order = c("degree", "no degree"))
```

Then, generating a bootstrap distribution,

``` r
boot_dist <- gss |>
   specify(hours ~ college) |>
   generate(reps = 1000, type = "bootstrap") |>
   calculate(stat = "t", order = c("degree", "no degree"))
```

Use the bootstrap distribution to find a confidence interval,

``` r
percentile_ci <- get_ci(boot_dist)
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-159-1.png)

Alternatively, use the bootstrap distribution to find a confidence
interval using the standard error,

``` r
standard_error_ci <- boot_dist |>
  get_ci(type = "se", point_estimate = t_hat)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-160-1.png)

See the above subsection (diff in means) for a theory-based approach.
`infer` does not support confidence intervals on means via the `z`
distribution.

### Two categorical variables (diff in proportions)

Finding the observed statistic,

``` r
d_hat <- gss |> 
  specify(college ~ sex, success = "degree") |>
  calculate(stat = "diff in props", order = c("female", "male"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
d_hat <- gss |> 
  observe(college ~ sex, success = "degree",
          stat = "diff in props", order = c("female", "male"))
```

Then, generating a bootstrap distribution,

``` r
boot_dist <- gss |>
  specify(college ~ sex, success = "degree") |>
  generate(reps = 1000, type = "bootstrap") |> 
  calculate(stat = "diff in props", order = c("female", "male"))
```

Use the bootstrap distribution to find a confidence interval,

``` r
percentile_ci <- get_ci(boot_dist)
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-165-1.png)

Alternatively, use the bootstrap distribution to find a confidence
interval using the standard error,

``` r
standard_error_ci <- boot_dist |>
  get_ci(type = "se", point_estimate = d_hat)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-166-1.png)

Instead of a simulation-based bootstrap distribution, we can also define
a theory-based sampling distribution,

``` r
sampling_dist <- gss |> 
  specify(college ~ sex, success = "degree") |>
   assume(distribution = "z")
```

Visualization and calculation of confidence intervals interfaces in the
same way as with the simulation-based distribution,

``` r
theor_ci <- get_ci(sampling_dist, point_estimate = d_hat)

theor_ci
```

    ## # A tibble: 1 × 2
    ##   lower_ci upper_ci
    ##      <dbl>    <dbl>
    ## 1  -0.0794   0.0878

``` r
visualize(sampling_dist) +
  shade_confidence_interval(endpoints = theor_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-168-1.png)

Note that the `z` distribution is recentered and rescaled to lie on the
scale of the observed data.

### Two categorical variables (z)

Finding the standardized point estimate,

``` r
z_hat <- gss |> 
  specify(college ~ sex, success = "degree") |>
  calculate(stat = "z", order = c("female", "male"))
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
z_hat <- gss |> 
  observe(college ~ sex, success = "degree",
          stat = "z", order = c("female", "male"))
```

Then, generating a bootstrap distribution,

``` r
boot_dist <- gss |>
  specify(college ~ sex, success = "degree") |>
  generate(reps = 1000, type = "bootstrap") |> 
  calculate(stat = "z", order = c("female", "male"))
```

Use the bootstrap distribution to find a confidence interval,

``` r
percentile_ci <- get_ci(boot_dist)
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-173-1.png)

Alternatively, use the bootstrap distribution to find a confidence
interval using the standard error,

``` r
standard_error_ci <- boot_dist |>
  get_ci(type = "se", point_estimate = z_hat)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-174-1.png)

See the above subsection (diff in props) for a theory-based approach.

### Two numerical vars - SLR

Finding the observed statistic,

``` r
slope_hat <- gss |> 
  specify(hours ~ age) |>
  calculate(stat = "slope")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
slope_hat <- gss |> 
  observe(hours ~ age, stat = "slope")
```

Then, generating a bootstrap distribution,

``` r
boot_dist <- gss |>
   specify(hours ~ age) |> 
   generate(reps = 1000, type = "bootstrap") |>
   calculate(stat = "slope")
```

Use the bootstrap distribution to find a confidence interval,

``` r
percentile_ci <- get_ci(boot_dist)
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-179-1.png)

Alternatively, use the bootstrap distribution to find a confidence
interval using the standard error,

``` r
standard_error_ci <- boot_dist |>
  get_ci(type = "se", point_estimate = slope_hat)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-180-1.png)

### Two numerical vars - correlation

Finding the observed statistic,

``` r
correlation_hat <- gss |> 
  specify(hours ~ age) |>
  calculate(stat = "correlation")
```

Alternatively, using the
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md)
wrapper to calculate the observed statistic,

``` r
correlation_hat <- gss |> 
  observe(hours ~ age, stat = "correlation")
```

Then, generating a bootstrap distribution,

``` r
boot_dist <- gss |>
   specify(hours ~ age) |> 
   generate(reps = 1000, type = "bootstrap") |>
   calculate(stat = "correlation")
```

Use the bootstrap distribution to find a confidence interval,

``` r
percentile_ci <- get_ci(boot_dist)
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-185-1.png)

Alternatively, use the bootstrap distribution to find a confidence
interval using the standard error,

``` r
standard_error_ci <- boot_dist |>
  get_ci(type = "se", point_estimate = correlation_hat)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-186-1.png)

### Two numerical vars - t

Not currently implemented since $t$ could refer to standardized slope or
standardized correlation.

### Multiple explanatory variables

Calculating the observed fit,

``` r
obs_fit <- gss |>
  specify(hours ~ age + college) |>
  fit()
```

Then, generating a bootstrap distribution,

``` r
boot_dist <- gss |>
  specify(hours ~ age + college) |>
  generate(reps = 1000, type = "bootstrap") |>
  fit()
```

Use the bootstrap distribution to find a confidence interval,

``` r
conf_ints <- 
  get_confidence_interval(
    boot_dist, 
    level = .95, 
    point_estimate = obs_fit
  )
```

Visualizing the observed statistic alongside the distribution,

``` r
visualize(boot_dist) +
  shade_confidence_interval(endpoints = conf_ints)
```

![](observed_stat_examples_files/figure-html/unnamed-chunk-190-1.png)

Note that this
[`fit()`](https://generics.r-lib.org/reference/fit.html)-based workflow
can be applied to use cases with differing numbers of explanatory
variables and explanatory variable types.
