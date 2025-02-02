% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DexiValues.R
\name{scale_values}
\alias{scale_values}
\title{scale_values}
\usage{
scale_values(values, scale)
}
\arguments{
\item{values}{A list of values. For possible value types, see \code{\link[=scale_value]{scale_value()}}.}

\item{scale}{A \link{DexiScale} or derived object.}
}
\value{
A list determined as \code{lapply(values, function (v) scale_value(v, scale))}.
}
\description{
A vectorized version of \code{scale_value}.
}
\seealso{
\code{\link[=scale_value]{scale_value()}}
}
