% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DexiCharts.R
\name{ggplot_parallel}
\alias{ggplot_parallel}
\title{ggplot_parallel}
\usage{
ggplot_parallel(
  model,
  alternatives = NULL,
  attids = NULL,
  aggregate = c("minmax", "min", "max", "mean", "none"),
  name = "name",
  shift = 0.01
)
}
\arguments{
\item{model}{A \link{DexiModel} object. Required.}

\item{alternatives}{A \code{data.frame} of alternatives (normally an output of \code{\link[=evaluate]{evaluate()}}) or
indices to \code{model$alternatives}. The default value \code{NULL} selects the whole \code{model$alternatives}.}

\item{attids}{\code{character()}. A character vector of \link{DexiAttribute} IDs to be included in the result.
Default: all \code{model} attributes.}

\item{aggregate}{One of \code{"minmax"}, \code{"min"}, \code{"max"}, \code{"mean"} or "\code{none}". Determines how
to aggregate \code{alternatives}' values that are represented by sets or distributions.}

\item{name}{\code{character(1)}, The name of the column in \code{alternatives} that contains
alternatives' names. Default: \code{"name"}.}

\item{shift}{\code{numeric(1)}. Used to "shift" numeric values by a small amount to avoid overlapping
lines in charts. Default: \code{0.01}.
You may want to experiment with charts to determine the right value,}
}
\value{
A basic 'ggplot2' chart. Generally, this chart needs to be further enhanced
by graph layers, such as themes, labels, \code{geom_points()} and \code{geom_line()}.
See \code{\link[=plotalt_parallel]{plotalt_parallel()}} that already provides some such layers.
}
\description{
Makes a basic \code{ggplot2} chart for displaying DEXi alternatives using parallel axes.
Generally, axes are uniformly scaled to the \verb{[0,1]} interval.
}
\details{
Uses \code{\link[GGally:ggparcoord]{GGally::ggparcoord()}} and requires package "GGally" to be installed.
Data presented in the chart is prepared by \code{\link[=scale_alternatives]{scale_alternatives()}}.
}
\examples{
if (requireNamespace("GGally", quietly = TRUE)) {

# Load "Car.dxi"
CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
Car <- read_dexi(CarDxi)

# Plot all Car$alternatives with points and lines
ggplot_parallel(Car) + ggplot2::geom_line(linewidth = 2) + ggplot2::geom_point(size = 3)
}
}
\seealso{
\code{\link[=scale_alternatives]{scale_alternatives()}}, \code{\link[=plotalt_parallel]{plotalt_parallel()}}
}
