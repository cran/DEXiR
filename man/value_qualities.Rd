% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DexiValues.R
\name{value_qualities}
\alias{value_qualities}
\title{value_qualities}
\usage{
value_qualities(value, scale)
}
\arguments{
\item{value}{A DEXi value, internal representation: numeric value or vector, or \link{distribution}.}

\item{scale}{A \link{DexiScale} or derived object.}
}
\value{
A vector consisting of \code{EnumQuality} elements corresponding to individual \code{value} elements.
}
\description{
Returns a vector of qualities corresponding to consecutive elements of \code{value}.
In contrast with \code{DexiScale$value_quality(value)}, which can handle only single values,
this function can handle \code{value} arguments that contain multiple elements,
such as value sets and distributions.
}
\examples{
scl <- DexiDiscreteScale(values = c("low", "med", "high"))
value_qualities(1, scl)       # "bad"
value_qualities(1:3, scl)     # c("bad", "none", "good")
value_qualities(c(3, 2), scl) # c("good", "none")
}
