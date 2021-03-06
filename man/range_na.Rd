% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bitsort.R
\name{range_na}
\alias{range_na}
\title{Get range and number of NAs}
\usage{
range_na(x)
}
\arguments{
\item{x}{an integer vector}
}
\value{
an integer vector with three elements \describe{
  \item{ 1 }{ min integer }
  \item{ 2 }{ max integer }
  \item{ 3 }{ number of NAs }
  }
}
\description{
Get range and number of NAs
}
\examples{
range_na(c(0L,1L,2L,NA))
}
\seealso{
\code{\link{range_nanozero}} and \code{\link{range_sortna}}
}
