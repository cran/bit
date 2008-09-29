\name{Extract}
\alias{[[.bit}
\alias{[[<-.bit}
\alias{[.bit}
\alias{[<-.bit}
\title{ Extract or replace part of an bit vector }
\description{
  Operators acting on bit objects to extract or replace parts.
}
\usage{
\method{[[}{bit}(x, i)
\method{[[}{bit}(x, i) <- value
\method{[}{bit}(x, i)
\method{[}{bit}(x, i) <- value
}
\arguments{
  \item{x}{ a bit object }
  \item{i}{ positive integer subscript }
  \item{value}{ new logical or integer values }
}
\details{
  Since this package was created for high performance purposes, only positive integer subscripts are allowed.
  The '[' and '[<-' methods don't check whether the subscripts are positive integers in the allowed range.
}
\value{
  The extractors \code{[[} and \code{[} return a logical scalar or vector.
  The replacment functions return a bit object.
}
\author{ Jens Oehlschlägel }
\seealso{ \code{\link{bit}}, \code{\link{Extract}} }
\examples{
  x <- as.bit(c(FALSE, NA, TRUE))
  x[] <- c(FALSE, NA, TRUE)
  x[1:2]
  x[[1]]
  x[] <- TRUE
  x[1:2] <- FALSE
  x[[1]] <- TRUE
}
\keyword{ classes }
\keyword{ logic }
