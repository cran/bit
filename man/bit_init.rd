% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bit.R
\docType{data}
\name{.BITS}
\alias{.BITS}
\alias{bit_init}
\alias{bit_done}
\title{Initializing bit masks}
\format{
An object of class \code{integer} of length 1.
}
\usage{
.BITS

bit_init()

bit_done()
}
\description{
Functions to allocate (and de-allocate) bit masks
}
\details{
The C-code operates with bit masks.  The memory for these is allocated
dynamically.  \code{bit_init} is called by \code{\link{.First.lib}} and
\code{bit_done} is called by \code{\link{.Last.lib}}.  You don't need to
care about these under normal circumstances.
}
\examples{

  bit_done()
  bit_init()

}
\seealso{
\code{\link{bit}}
}
\author{
Jens Oehlschlägel
}
\keyword{classes}
\keyword{datasets}
\keyword{logic}
