# Subset of data from the General Social Survey (GSS).

The General Social Survey is a high-quality survey which gathers data on
American society and opinions, conducted since 1972. This data set is a
sample of 500 entries from the GSS, spanning years 1973-2018, including
demographic markers and some economic variables. Note that this data is
included for demonstration only, and should not be assumed to provide
accurate estimates relating to the GSS. However, due to the high quality
of the GSS, the unweighted data will approximate the weighted data in
some analyses.

## Usage

``` r
gss
```

## Format

A tibble with 500 rows and 11 variables:

- year:

  year respondent was surveyed

- age:

  age at time of survey, truncated at 89

- sex:

  respondent's sex (self-identified)

- college:

  whether on not respondent has a college degree, including
  junior/community college

- partyid:

  political party affiliation

- hompop:

  number of persons in household

- hours:

  number of hours worked in week before survey, truncated at 89

- income:

  total family income

- class:

  subjective socioeconomic class identification

- finrela:

  opinion of family income

- weight:

  survey weight

## Source

<https://gss.norc.org>
