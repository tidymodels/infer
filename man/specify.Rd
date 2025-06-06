% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specify.R
\name{specify}
\alias{specify}
\title{Specify response and explanatory variables}
\usage{
specify(x, formula, response = NULL, explanatory = NULL, success = NULL)
}
\arguments{
\item{x}{A data frame that can be coerced into a \link[tibble:tibble]{tibble}.}

\item{formula}{A formula with the response variable on the left and the
explanatory on the right. Alternatively, a \code{response} and \code{explanatory}
argument can be supplied.}

\item{response}{The variable name in \code{x} that will serve as the response.
This is an alternative to using the \code{formula} argument.}

\item{explanatory}{The variable name in \code{x} that will serve as the
explanatory variable. This is an alternative to using the formula argument.}

\item{success}{The level of \code{response} that will be considered a success, as
a string. Needed for inference on one proportion, a difference in
proportions, and corresponding z stats.}
}
\value{
A tibble containing the response (and explanatory, if specified)
variable data.
}
\description{
\code{specify()} is used to specify which columns in the supplied data frame are
the relevant response (and, if applicable, explanatory) variables. Note that
character variables are converted to \code{factor}s.

Learn more in \code{vignette("infer")}.
}
\examples{
# specifying for a point estimate on one variable
gss |>
   specify(response = age)

# specify a relationship between variables as a formula...
gss |>
  specify(age ~ partyid)

# ...or with named arguments!
gss |>
  specify(response = age, explanatory = partyid)

# more in-depth explanation of how to use the infer package
\dontrun{
vignette("infer")
}

}
\seealso{
Other core functions: 
\code{\link{calculate}()},
\code{\link{generate}()},
\code{\link{hypothesize}()}
}
\concept{core functions}
