\name{length.bit}
\alias{length.bit}
\alias{length<-.bit}
\title{ Getting and setting length of a bit vector }
\description{
  Query the number of bits in a bit vector or change the number of bits in a bit vector.
}
\usage{
\method{length}{bit}(x)
\method{length}{bit}(x) <- value
}
\arguments{
  \item{x}{ a bit vector }
  \item{value}{ the new number of bits }
}
\details{
  Note that no explicit initialization is done.
  As a consequence, when you first decrease and then increase length you might find that some 'new' bits are already \code{TRUE}.
}
\value{
  A bit vector with the new length
}
\author{ Jens Oehlschlägel }
\seealso{ \code{\link{bit}}, \code{\link{length}}  }
\examples{
  x <- bit(32)
  length(x)
  x[c(1, 32)] <- TRUE
  length(x) <- 16
  x
  length(x) <- 32
  x
}
\keyword{ classes }
\keyword{ logic }
