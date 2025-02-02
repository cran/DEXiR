% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DexiUtils.R
\name{reverse_value}
\alias{reverse_value}
\title{reverse_value}
\usage{
reverse_value(x, lb, hb)
}
\arguments{
\item{x}{\code{numeric()}. Value(s) to be reversed.}

\item{lb}{\code{numeric()}. Lower interval bound(s).}

\item{hb}{\code{numeric()}. Upper interval bound(s).}
}
\value{
\code{numeric()}. Reversed \code{value}.
}
\description{
Numeric value(s) \code{x} are assumed to lie within the \verb{[lb:hb]} interval.
The function "reverses" \code{x} linearly so that \code{x = lb} maps to \code{hb} and \code{x = hb} maps to \code{lb}.
In DEXiR, this function is used to reverse values defined on a \link{DexiScale} from
"ascending" to "descending" order or vice versa.
}
\examples{
reverse_value(1, 1, 5) # 5
reverse_value(3, 1, 5) # 3
reverse_value(5, 1, 5) # 1
reverse_value(c(1, 3, 5), 1, 5) # c(5, 3, 1)
}
