% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DexiValues.R
\name{distr_to_set}
\alias{distr_to_set}
\title{distr_to_set}
\usage{
distr_to_set(distr, eps = .Machine$double.eps)
}
\arguments{
\item{distr}{An S3 object of class \code{distribution}.}

\item{eps}{A numeric value representing the threshold value of \eqn{p} (see \link{DEXiR-package})
above which the corresponding elements are considered set members.}
}
\value{
A numeric vector determined as \code{which(distr > eps)}. Notice that \code{distr_to_set}
is generally a lossy conversion, so that multiple different \code{distr}s are converted to the same sets.
}
\description{
Convert a DEXi value distribution to a DEXi value set.
}
\examples{
distr_to_set(distribution(0.2, 0, 0.5, 0.3))
distr_to_set(distribution(0.1, 0, 0.7, 0.2))
distr_to_set(distribution(0.1, 0, 0.7, 0.2), eps = 0.5)

}
\seealso{
\link{DEXiR-package}, \link{distribution}, \code{\link[=set_to_distr]{set_to_distr()}}
}
