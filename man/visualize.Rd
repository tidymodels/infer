% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{visualize}
\alias{visualize}
\alias{visualise}
\title{Visualize statistical inference}
\usage{
visualize(data, bins = 15, method = "simulation", dens_color = "black", ...)

visualise(data, bins = 15, method = "simulation", dens_color = "black", ...)
}
\arguments{
\item{data}{A distribution of statistics (for \code{\link[=calculate]{calculate()}}-based workflows)
or coefficient estimates (for \code{\link[=fit.infer]{fit()}}-based workflows)
outputted by infer.}

\item{bins}{The number of bins in the histogram.}

\item{method}{A string giving the method to display. Options are
\code{"simulation"}, \code{"theoretical"}, or \code{"both"} with \code{"both"} corresponding to
\code{"simulation"} and \code{"theoretical"}.}

\item{dens_color}{A character or hex string specifying the color of the
theoretical density curve.}

\item{...}{Other arguments passed along to \\{ggplot2\\} functions.}
}
\value{
For \code{\link[=calculate]{calculate()}}-based workflows, a ggplot object showing the
simulation-based distribution as a histogram or bar graph. Also used to
show the theoretical curves.

For \code{\link[=fit.infer]{fit()}}-based workflows, a \code{patchwork} object
showing the simulation-based distributions as a histogram or bar graph.
The interface to adjust plot options and themes is a bit different
for \code{patchwork} plots than ggplot2 plots. The examples highlight the
biggest differences here, but see \code{\link[patchwork:plot_annotation]{patchwork::plot_annotation()}} and
\link[patchwork:plot_arithmetic]{patchwork::&.gg} for more details.
}
\description{
Visualize the distribution of the simulation-based inferential statistics or
the theoretical distribution (or both!).

Learn more in \code{vignette("infer")}.
}
\details{
In order to make the visualization workflow more straightforward
and explicit, \code{visualize()} now only should be used to plot distributions
of statistics directly. A number of arguments related to shading p-values and
confidence intervals are now deprecated in \code{visualize()} and should
now be passed to \code{\link[=shade_p_value]{shade_p_value()}} and \code{\link[=shade_confidence_interval]{shade_confidence_interval()}},
respectively. \code{\link[=visualize]{visualize()}} will raise a warning if deprecated arguments
are supplied.
}
\examples{
  
# find a null distribution
null_dist <- gss \%>\%
  # we're interested in the number of hours worked per week
  specify(response = hours) \%>\%
  # hypothesizing that the mean is 40
  hypothesize(null = "point", mu = 40) \%>\%
  # generating data points for a null distribution
  generate(reps = 1000, type = "bootstrap") \%>\%
  # calculating a distribution of t test statistics
  calculate(stat = "t")
  
# we can easily plot the null distribution by piping into visualize
null_dist \%>\%
  visualize()

# we can add layers to the plot as in ggplot, as well... 
# find the point estimate---mean number of hours worked per week
point_estimate <- gss \%>\%
  specify(response = hours) \%>\%
  hypothesize(null = "point", mu = 40) \%>\%
  calculate(stat = "t")
  
# find a confidence interval around the point estimate
ci <- null_dist \%>\%
  get_confidence_interval(point_estimate = point_estimate,
                          # at the 95\% confidence level
                          level = .95,
                          # using the standard error method
                          type = "se")  
  
# display a shading of the area beyond the p-value on the plot
null_dist \%>\%
  visualize() +
  shade_p_value(obs_stat = point_estimate, direction = "two-sided")

null_dist \%>\%
  visualize() +
  shade_confidence_interval(ci)
  
# to plot a theoretical null distribution, skip the generate()
# step and supply `method = "theoretical"` to `visualize()`
null_dist_theoretical <- gss \%>\%
  specify(response = hours) \%>\%
  hypothesize(null = "point", mu = 40) \%>\%
  calculate(stat = "t") 
  
visualize(null_dist_theoretical, method = "theoretical")

# to plot both a theory-based and simulation-based null distribution,
# use the simulation-based null distribution and supply
# `method = "both"` to `visualize()`
visualize(null_dist, method = "both")

\donttest{
# to visualize distributions of coefficients for multiple
# explanatory variables, use a `fit()`-based workflow

# fit 1000 models with the `hours` variable permuted
null_fits <- gss \%>\%
 specify(hours ~ age + college) \%>\%
 hypothesize(null = "independence") \%>\%
 generate(reps = 1000, type = "permute") \%>\%
 fit()
 
null_fits

# visualize distributions of resulting coefficients
visualize(null_fits)

# the interface to add themes and other elements to patchwork
# plots (outputted by `visualize` when the inputted data
# is from the `fit()` function) is a bit different than adding
# them to ggplot2 plots.
library(ggplot2)

# to add a ggplot2 theme to a `calculate()`-based visualization, use `+`
null_dist \%>\% visualize() + theme_dark()
  
# to add a ggplot2 theme to a `fit()`-based visualization, use `&`
null_fits \%>\% visualize() & theme_dark()
}

# More in-depth explanation of how to use the infer package
\dontrun{
vignette("infer")
}

}
\seealso{
\code{\link[=shade_p_value]{shade_p_value()}}, \code{\link[=shade_confidence_interval]{shade_confidence_interval()}}.
}
