\name{as.bit}
\alias{is.bit}
\alias{as.bit}
\alias{as.bit.bit}
\alias{as.bit.logical}
\alias{as.bit.integer}
\alias{as.bit.which}
\title{ Coercion to bit }
\description{
  Coerces logical or bit to bit vector (and test for bit)
}
\usage{
is.bit(x)
as.bit(x, \dots)
\method{as.bit}{bit}(x, \dots)
\method{as.bit}{logical}(x, \dots)
\method{as.bit}{integer}(x, \dots)
\method{as.bit}{which}(x, length, \dots)
}
\arguments{
  \item{x}{ an object of class bit, logical or integer }
  \item{length}{ the length of the new bit vector }
  \item{\dots}{ further arguments }
}
\details{
  Coercion to bit is quite fast because we use a double loop that fixes each word in a processor register
}
\note{
  Zero is coerced to FALSE, all other numbers including NA are coerced to TRUE.
  This differs from the NA-to-FALSE coercion in package ff and may change in the future.
}
\value{
  \code{is.bit} returns FALSE or TRUE, \code{as.bit} returns a vector of class 'bit'
}
\author{ Jens Oehlschlägel }
\seealso{ \code{\link{bit}}, \code{\link[bit:as.logical.bit]{as.logical}} }
\examples{
  x <- as.bit(c(FALSE, NA, TRUE))
  is.bit(x)
  as.bit(x)
  as.bit.which(c(1,3,4), 12)
}
\keyword{ classes }
\keyword{ logic }
