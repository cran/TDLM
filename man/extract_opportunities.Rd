% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_opportunities.R
\name{extract_opportunities}
\alias{extract_opportunities}
\title{Compute the number of opportunities between pairs of locations}
\usage{
extract_opportunities(opportunity, distance, check_names = FALSE)
}
\arguments{
\item{opportunity}{a numeric vector representing the number of opportunities
per location. The value should be positive.}

\item{distance}{a squared matrix representing the distance between locations.}

\item{check_names}{a boolean indicating if the ID location are used as
vector names, matrix rownames and colnames and if they should be checked
(see Note).}
}
\value{
A squared matrix in which each element represents the number of opportunities
between a pair of locations.
}
\description{
This function computes the number of opportunities between pairs of locations
as defined in \insertCite{Lenormand2016;textual}{TDLM}.
For a given pair of location the number of opportunities between the location
of origin and the location of destination is based on the number of
opportunities in a circle of radius equal to the distance between origin and
destination centered in the origin. The number of opportunities at origin
and destination are not included.
}
\note{
\code{opportunity} and \code{distance} should be based on the same number of
locations sorted in the same order. It is recommended to use the location ID
as vector names, matrix rownames and matrix colnames and to set
\code{check_names = TRUE} to verify that everything is in order before running
this function (\code{check_names = FALSE} by default). Note that the function
\code{\link[=check_format_names]{check_format_names()}} can be used to control the validity of all the inputs
before running the main package's functions.
}
\examples{
data(mass)
data(distance)

opportunity <- mass[, 1]

sij <- extract_opportunities(
  opportunity = opportunity,
  distance = distance,
  check_names = FALSE
)

}
\references{
\insertRef{Lenormand2016}{TDLM}
}
\seealso{
\code{\link[=calib_param]{calib_param()}} \code{\link[=extract_spatial_information]{extract_spatial_information()}}
\code{\link[=check_format_names]{check_format_names()}}
}
\author{
Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
}
