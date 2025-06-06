% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate.R
\name{calculate}
\alias{calculate}
\title{Calculate summary statistics}
\usage{
calculate(
  x,
  stat = c("mean", "median", "sum", "sd", "prop", "count", "diff in means",
    "diff in medians", "diff in props", "Chisq", "F", "slope", "correlation", "t", "z",
    "ratio of props", "odds ratio", "ratio of means"),
  order = NULL,
  ...
)
}
\arguments{
\item{x}{The output from \code{\link[=generate]{generate()}} for computation-based inference or the
output from \code{\link[=hypothesize]{hypothesize()}} piped in to here for theory-based inference.}

\item{stat}{A string giving the type of the statistic to calculate. Current
options include \code{"mean"}, \code{"median"}, \code{"sum"}, \code{"sd"}, \code{"prop"}, \code{"count"},
\code{"diff in means"}, \code{"diff in medians"}, \code{"diff in props"}, \code{"Chisq"} (or
\code{"chisq"}), \code{"F"} (or \code{"f"}), \code{"t"}, \code{"z"}, \code{"ratio of props"}, \code{"slope"},
\code{"odds ratio"}, \code{"ratio of means"}, and \code{"correlation"}. \code{infer} only
supports theoretical tests on one or two means via the \code{"t"} distribution
and one or two proportions via the \code{"z"}.}

\item{order}{A string vector of specifying the order in which the levels of
the explanatory variable should be ordered for subtraction (or division
for ratio-based statistics), where \code{order = c("first", "second")} means
\code{("first" - "second")}, or the analogue for ratios. Needed for inference on
difference in means, medians, proportions, ratios, t, and z statistics.}

\item{...}{To pass options like \code{na.rm = TRUE} into functions like
\link[base:mean]{mean()}, \link[stats:sd]{sd()}, etc. Can also be used to
supply hypothesized null values for the \code{"t"} statistic or additional
arguments to \code{\link[stats:chisq.test]{stats::chisq.test()}}.}
}
\value{
A tibble containing a \code{stat} column of calculated statistics.
}
\description{
Given the output of \code{\link[=specify]{specify()}} and/or \code{\link[=hypothesize]{hypothesize()}}, this function will
return the observed statistic specified with the \code{stat} argument. Some test
statistics, such as \code{Chisq}, \code{t}, and \code{z}, require a null hypothesis. If
provided the output of \code{\link[=generate]{generate()}}, the function will calculate the
supplied \code{stat} for each \code{replicate}.

Learn more in \code{vignette("infer")}.
}
\section{Missing levels in small samples}{

In some cases, when bootstrapping with small samples, some generated
bootstrap samples will have only one level of the explanatory variable
present. For some test statistics, the calculated statistic in these
cases will be NaN. The package will omit non-finite values from
visualizations (with a warning) and raise an error in p-value calculations.
}

\section{Reproducibility}{
When using the infer package for research, or in other cases when exact
reproducibility is a priority, be sure the set the seed for R’s random
number generator. infer will respect the random seed specified in the
\code{set.seed()} function, returning the same result when \code{generate()}ing
data given an identical seed. For instance, we can calculate the
difference in mean \code{age} by \code{college} degree status using the \code{gss}
dataset from 10 versions of the \code{gss} resampled with permutation using
the following code.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{set.seed(1)

gss |>
  specify(age ~ college) |>
  hypothesize(null = "independence") |>
  generate(reps = 5, type = "permute") |>
  calculate("diff in means", order = c("degree", "no degree"))
}\if{html}{\out{</div>}}

\if{html}{\out{<div class="sourceCode">}}\preformatted{## Response: age (numeric)
## Explanatory: college (factor)
## Null Hypothesis: indepe...
## # A tibble: 5 x 2
##   replicate   stat
##       <int>  <dbl>
## 1         1 -0.531
## 2         2 -2.35 
## 3         3  0.764
## 4         4  0.280
## 5         5  0.350
}\if{html}{\out{</div>}}

Setting the seed to the same value again and rerunning the same code
will produce the same result.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{# set the seed
set.seed(1)

gss |>
  specify(age ~ college) |>
  hypothesize(null = "independence") |>
  generate(reps = 5, type = "permute") |>
  calculate("diff in means", order = c("degree", "no degree"))
}\if{html}{\out{</div>}}

\if{html}{\out{<div class="sourceCode">}}\preformatted{## Response: age (numeric)
## Explanatory: college (factor)
## Null Hypothesis: indepe...
## # A tibble: 5 x 2
##   replicate   stat
##       <int>  <dbl>
## 1         1 -0.531
## 2         2 -2.35 
## 3         3  0.764
## 4         4  0.280
## 5         5  0.350
}\if{html}{\out{</div>}}

Please keep this in mind when writing infer code that utilizes
resampling with \code{generate()}.
}

\examples{

# calculate a null distribution of hours worked per week under
# the null hypothesis that the mean is 40
gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40) |>
  generate(reps = 200, type = "bootstrap") |>
  calculate(stat = "mean")

# calculate the corresponding observed statistic
gss |>
  specify(response = hours) |>
  calculate(stat = "mean")

# calculate a null distribution assuming independence between age
# of respondent and whether they have a college degree
gss |>
  specify(age ~ college) |>
  hypothesize(null = "independence") |>
  generate(reps = 200, type = "permute") |>
  calculate("diff in means", order = c("degree", "no degree"))

# calculate the corresponding observed statistic
gss |>
  specify(age ~ college) |>
  calculate("diff in means", order = c("degree", "no degree"))

# some statistics require a null hypothesis
 gss |>
   specify(response = hours) |>
   hypothesize(null = "point", mu = 40) |>
   calculate(stat = "t")

# more in-depth explanation of how to use the infer package
\dontrun{
vignette("infer")
}

}
\seealso{
\code{\link[=visualize]{visualize()}}, \code{\link[=get_p_value]{get_p_value()}}, and \code{\link[=get_confidence_interval]{get_confidence_interval()}}
to extract value from this function's outputs.

Other core functions: 
\code{\link{generate}()},
\code{\link{hypothesize}()},
\code{\link{specify}()}
}
\concept{core functions}
