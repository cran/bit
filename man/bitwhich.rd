% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bit.R
\name{bitwhich}
\alias{bitwhich}
\title{Create bitwhich vector (skewed boolean)}
\usage{
bitwhich(
  maxindex = 0L,
  x = NULL,
  xempty = FALSE,
  poslength = NULL,
  is.unsorted = TRUE,
  has.dup = TRUE
)
}
\arguments{
\item{maxindex}{length of the vector}

\item{x}{Information about which positions are FALSE or TRUE: either \code{logical()} or \code{TRUE} or \code{FALSE} or a integer vector of positive or of negative subscripts.}

\item{xempty}{what to assume about parameter \code{x} if \code{x=integer(0)}, typically \code{TRUE} or \code{FALSE}.}

\item{poslength}{tuning: \code{poslength} is calculated automatically, you can give \code{poslength} explicitely, in this case it must be correct and \code{x} must be sorted and not have duplicates.}

\item{is.unsorted}{tuning: FALSE implies that \code{x} is already sorted and sorting is skipped}

\item{has.dup}{tuning: FALSE implies that \code{x} has no duplicates}
}
\value{
an object of class 'bitwhich' carrying two attributes
\describe{
  \item{maxindex}{ see above }
  \item{poslength}{ see above }
}
}
\description{
A bitwhich object represents a boolean filter like a \code{\link{bit}} object (NAs are not allowed)
but uses a sparse representation suitable for very skewed (asymmetric) selections. 
Three extreme cases are represented with logical values, no length via logical(), 
all TRUE with TRUE and all FALSE with FALSE. All other selections are represented with 
positive or negative integers, whatever is shorter. 
This needs less RAM compared to \code{\link{logical}} (and often less than \code{\link{bit}} or \code{\link[=as.which]{which}}).
Logical operations are fast if the selection is asymetric (only few or almost all selected).
}
\examples{
bitwhich()
bitwhich(12)
bitwhich(12, x=TRUE)
bitwhich(12, x=3)
bitwhich(12, x=-3)
bitwhich(12, x=integer())
bitwhich(12, x=integer(), xempty=TRUE)
}
\seealso{
\code{\link{bitwhich_representation}},  \code{\link{as.bitwhich}}, \code{\link{bit}}
}
