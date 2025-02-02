% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DexiCharts.R
\name{plotalt1}
\alias{plotalt1}
\title{plotalt1}
\usage{
plotalt1(
  model,
  attribute = model$first(),
  alternatives = NULL,
  colors = c("red", "black", "green"),
  pch = 20,
  size = 5,
  linetype = 2,
  margins = NULL,
  lm = NULL,
  ...
)
}
\arguments{
\item{model}{A \link{DexiModel} object. Required.}

\item{attribute}{A single \link{DexiAttribute} selector. It may be an \link{DexiAttribute} object or
an argument to \code{model$attrib()}. \code{attribute$scale} must be defined. Default: \code{model$first()}.}

\item{alternatives}{A \code{data.frame} of alternatives (normally an output of \code{\link[=evaluate]{evaluate()}}) or
indices to \code{model$alternatives}. The default value \code{NULL} selects the whole \code{model$alternatives}.}

\item{colors}{\code{character(3)} representing colors corresponding to "bad", "neutral" and "good"
scale values, respectively. Default: \code{c("red", "black", "green")}.}

\item{pch}{Plotting character, see \code{\link[graphics:points]{graphics::points()}}. Default: \code{20}.}

\item{size}{\code{numeric(1)}. Multiplication size factor for drawing individual points.
Base point size depends on \code{pch}.}

\item{linetype}{\code{integer()}. Line type for drawing chart grid. Default: \code{2}.}

\item{margins}{\code{numeric(4)}. Chart margins, passed to \code{\link[graphics:par]{graphics::par()}} prior to drawing.}

\item{lm}{\code{numeric(1)}. Left chart margin. May be used to adjust the display of alternatives' names.}

\item{...}{Optional parameters passed to \code{\link[graphics:plot.default]{graphics::plot()}}.}
}
\value{
Draws a chart.
}
\description{
Plot \code{alternatives} with respect to a single \code{attribute}.
}
\details{
Standard scatterplot \link[base:plot]{base::plot} is used.
}
\examples{
# Load "Car.dxi"
CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
Car <- read_dexi(CarDxi)

# Plot all Car$alternatives with respect to "TECH.CHAR." attribute
plotalt1(Car, "TECH.CHAR.")

# Plot the first Car alternative with respect to "MAINT.PRICE" attribute
plotalt1(Car, "MAINT.PRICE", 1)

}
