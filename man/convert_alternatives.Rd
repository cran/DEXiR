% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DexiAlternatives.R
\name{convert_alternatives}
\alias{convert_alternatives}
\title{convert_alternatives}
\usage{
convert_alternatives(
  model,
  alternatives = NULL,
  interpret = c("set", "distribution", "none"),
  aggregate = min,
  omin = 0,
  omax = 1,
  map_values = TRUE,
  reverse_descending = TRUE,
  verbatim = "name",
  skip = NULL,
  continuous = convert_data_continuous,
  discrete = convert_data_discrete
)
}
\arguments{
\item{model}{A \link{DexiModel} object. Required.}

\item{alternatives}{A \code{data.frame} of alternatives (normally an output of \code{\link[=evaluate]{evaluate()}}) or
indices to \code{model$alternatives}. The default value \code{NULL} selects \code{model$alternatives}.}

\item{interpret}{\code{character(1)}. Determines how the original values in \code{alternatives} are
interpreted, i.e., converted prior to submitting them to \code{aggregate()}:
\describe{
\item{\code{"set"}}{As a set of values. Any \link{distribution}-type value is converted to a set,
thus discarding the numeric membership information.}
\item{\code{"distribution"}}{As a value distribution, i.e., a numeric vector of membership values.}
\item{\code{"none"}}{No conversion.}
}
Values corresponding to continuous attributes are not converted nor affected by these settings.}

\item{aggregate}{A function accepting the interpreted DEXi value (see \code{interpret}) and converting
it to become part of the output data frame. Normally, this function is assumed to accept
a numeric vector argument and aggregate it in a single numeric value.
The default aggregation function is \code{\link[=min]{min()}}. Typical alternatives include \code{\link[=max]{max()}} and \code{\link[=mean]{mean()}}.}

\item{omin}{\code{numeric(1)}. Lower bound of the output value interval (see \code{map_values}). Default: \code{0}.}

\item{omax}{\code{numeric(1)}. Upper bound of the output value interval (see \code{map_values}). Default: \code{1}.}

\item{map_values}{\verb{logical(1).} When \code{TRUE}, values produced by \code{aggregate()} are further scaled
to the interval \verb{[omin:omax]}. Input bounds are determined from
the corresponding attribute scales (for discrete attributes) or
as minimum/maximum values from \code{alternatives} (for continuous attributes).}

\item{reverse_descending}{\verb{logical(1).} Whether or not to reverse the values of attributes
whose scales are of a "descending" preference order.}

\item{verbatim}{\code{character()}. Names of \code{alternatives}' data columns
that are included in the output without conversion. Default: \code{"name"}.}

\item{skip}{\code{character()}. Names of \code{alternatives}' data columns
that are ignored in the process. Default: \code{NULL}.}

\item{continuous}{A function converting a data column that corresponds to a continuous attribute.
Default: \code{\link[=convert_data_continuous]{convert_data_continuous()}}.
Setting \code{continuous} to \code{NULL} excludes all continuous attributes from conversion.}

\item{discrete}{A function converting a data column that corresponds to a discrete attribute.
Default: \code{\link[=convert_data_discrete]{convert_data_discrete()}}.
Setting \code{discrete} to \code{NULL} excludes all discrete attributes from conversion.}
}
\value{
A converted \code{data.frame}.
}
\description{
Converts a \code{data.frame} of alternatives' data to another \code{data.frame}. The conversion
generally involves: aggregating DEXi values originally represented by sets or distributions,
scaling aggregated values to a given interval and/or reversing values assigned to "descending"
\link{DexiScale}s.
}
\details{
The rationale for \code{convert_alternatives()} is that data frames representing alternatives,
particularly those produced by \code{\link[=evaluate]{evaluate()}}, generally contain DEXi values of various
and mixed data types, such as numbers and numeric vectors (sets and distributions).
As such, this data is difficult to work with in R,
as most R functions expect simpler and more uniform data structures.
\code{convert_alternatives()} produces data frames that are more suitable for standard R data analysis
and graph drawing. However, as the conversion generally involves aggregation and mapping
of DEXi values, it may distort or lose information along the way.
}
\examples{
# Load "Car.dxi"
CarDxi <- system.file("extdata", "Car.dxi", package = "DEXiR")
Car <- read_dexi(CarDxi)

# Map Car$alternatives' values to the [0, 1] interval.
convert_alternatives(Car)

#  name     CAR.1 PRICE BUY.PRICE MAINT.PRICE TECH.CHAR. COMFORT X.PERS   X.DOORS LUGGAGE SAFETY
# 1 Car1 1.0000000   1.0       0.5         1.0  1.0000000       1      1 0.6666667       1    1.0
# 2 Car2 0.6666667   0.5       0.5         0.5  0.6666667       1      1 0.6666667       1    0.5

}
\seealso{
\code{\link[=convert_data_continuous]{convert_data_continuous()}}, \code{\link[=convert_data_discrete]{convert_data_discrete()}}, \code{\link[=scale_alternatives]{scale_alternatives()}},
\link{DEXiR-package} notes on values in DEXi models.
}
