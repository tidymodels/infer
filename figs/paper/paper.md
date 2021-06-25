---
title: 'infer: An R package for tidyverse-friendly statistical inference'
tags:
  - data science
  - tidyverse
  - inference
  - R
authors:
- name: Simon P. Couch
  orcid: 0000-0001-5676-5107
  affiliation: "1, 2"
- name: Andrew P. Bray
  orcid: 0000-0002-4037-7414
  affiliation: 3
- name: Chester Ismay
  orcid: 0000-0003-2820-2547
  affiliation: 4
- name: Evgeni Chasnovski
  orcid: 0000-0002-1617-4019
  affiliation: 5
- name: Benjamin S. Baumer
  orcid: 0000-0002-3279-0516
  affiliation: 6
- name: Mine Çetinkaya-Rundel
  orcid: 0000-0001-6452-2420
  affiliation: "2, 7"
affiliations:
 - name: Johns Hopkins, Department of Biostatistics
   index: 1
 - name: RStudio
   index: 2
 - name: UC Berkeley, Department of Statistics and Reed College Mathematics Department (on leave)
   index: 3
 - name: Flatiron School
   index: 4
 - name: No Affiliation
   index: 5
 - name: Smith College, Program in Statistical & Data Sciences
   index: 6
 - name: Duke University, Department of Statistical Science
   index: 7

citation_author: Couch et. al.
date: 12 June 2021
year: 2021
bibliography: paper.bib
output: 
  rticles::joss_article:
    keep_tex: true
    includes:
      in_header: columns.tex
csl: apa.csl
journal: JOSS
---

# Summary

`infer` implements an expressive grammar to perform statistical inference that adheres to the `tidyverse` design framework [@wickham2019welcome]. Rather than providing methods for specific statistical tests, this package consolidates the principles that are shared among common hypothesis tests and confidence intervals into a set of four main verbs (functions), supplemented with many utilities to visualize and extract value from their outputs.

# Statement of Need

Packages implementing methods for basic statistical inference in R are highly variable in their interfaces. The structure of inputted data, argument names, expected argument types, argument orders, output types, and spelling cases varies widely both within and among packages. This diversity in approaches obscures the intuition shared among common inferential procedures, makes details of usage difficult to remember, and prevents an expressive and idiomatic coding style.

`infer` is an R package for randomization-based hypothesis testing, naturalizing an intuitive understanding of statistical inference via a unified and expressive grammar. Four functions provide functionality encompassing a large swath of basic frequentist statistical inference, abstracting away details of specific tests and shifting the focus of the analyst to the observed data and the processes that generated it. Such a grammar lends itself to applications in teaching, data pedagogy research, applied scientific research, and advanced predictive modeling. For one, the principled approach of the package has made it an especially good fit for teaching introductory statistics and data science [@ismay2019statistical; @baumer2020teaching; @cetinkaya2021fresh] and research in data pedagogy [@fergusson2021introducing; @loy2021bringing]. Further, the package has already seen usage in a number of published scientific applications [@mclean2021controlled; @ask2021per; @fallon2021single]. Finally, the package integrates with the greater tidymodels collection of packages, a burgeoning software ecosystem for tidyverse-aligned predictive modeling used across many modern research and industrial applications [@kuhn2020tidymodels]. To date, the package has been downloaded more than 400,000 times.

# Underlying Principles

Regardless of the hypothesis test in question, an analyst asks the same kind of question when conducting statistical inference: is the effect/difference in the observed data real, or due to random chance? To answer this question, the analyst begins by assuming that the effect in the observed data was simply due to random chance, and calls this assumption the *null hypothesis*. (In reality, they might not believe in the null hypothesis at all---the null hypothesis is in opposition to the *alternate hypothesis*, which supposes that the effect present in the observed data is actually due to the fact that "something is going on.") The analyst then calculates a *test statistic* from the data that describes the observed effect. They can use this test statistic to calculate a *p-value* via juxtaposition with a *null distribution*, giving the probability that the observed data could come about if the null hypothesis were true. If this probability is below some pre-defined *significance level* $\alpha$, then the analyst can reject the null hypothesis.

The workflow of this package is designed around this idea. Starting out with some dataset,

+ `specify()` allows the analyst to specify the variable, or relationship between variables, that they are interested in.
+ `hypothesize()` allows the analyst to declare the null hypothesis.
+ `generate()` allows the analyst to generate data reflecting the null hypothesis or using the bootstrap.
+ `calculate()` allows the analyst to calculate summary statistics, either from
     * the observed data, to form the observed test statistic.
     * data `generate()`d to reflect the null hypothesis, to form a randomization-based null distribution of test statistics.

As such, the ultimate output of an infer pipeline using these four functions is generally an _observed statistic_ or _null distribution_ of test statistics. These four functions are thus supplemented with several utilities to visualize and extract value from their outputs.

+ `visualize()` plots the null distribution of test statistics.
     * `shade_p_value()` situates the observed statistic in the null distribution, shading the region as or more extreme.
+ `get_p_value()` calculates a p-value via the juxtaposition of the test statistic and null distribution.

The workflow outlined above can also be used for constructing confidence intervals via bootstrapping with the omission of the `hypothesize()` step in the pipeline. The resulting bootstrap distribution can then be visualized with `visualize()`, the confidence interval region can be situated in the bootstrap distribution with `shade_confidence_interval()`, and the bounds of the confidence interval can be calculated with `get_confidence_interval()`.

Beyond this, the package offers:

* methods for inference using theory-based distributions
* shorthand wrappers for common statistical tests using tidy data
* model-fitting workflows to accommodate multiple explanatory variables

# Comparison to Other Packages

`broom` and `parameters` convert model objects to unified output formats, though do not provide methods for fitting models, describing null distributions, performing bootstrapping, or calculating summary statistics from tabular data. `statsexpressions`, and adjacent packages in the `easystats` ecosystem, implement wrappers with consistent interfaces for theory-based hypothesis tests. At a higher level, the structure of each of these packages is defined by model types, where each model type has its own associated function and/or model object. In contrast, `infer` is structured around four functions, situating statistics and model types within a more abstracted grammar. `infer`'s initial release on the Comprehensive R Archive Network predated that of each of the packages mentioned above, with the exception of `broom`, a package situated in the same `tidymodels` ecosystem [@CRAN].

# Acknowledgements

We acknowledge contributions from Albert Y. Kim, Jo Hardin, Jay Lee, Amelia McNamara, Nick Solomon, and Richie Cotton.

# References
