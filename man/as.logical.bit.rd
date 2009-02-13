\name{as.logical.bit}
\alias{as.logical.bit}
\alias{as.integer.bit}
\alias{which}
\alias{which.default}
\alias{which.bit}
\title{ Coercion from bit }
\description{
  Coercing from bit to logical or integer
}
\usage{
\method{as.logical}{bit}(x, \dots)
\method{as.integer}{bit}(x, \dots)
which(x, \dots)
\method{which}{default}(x, arr.ind = FALSE, \dots)
\method{which}{bit}(x, negative = NA, \dots)
}
\arguments{
  \item{x}{ an object of class bit }
  \item{arr.ind}{ see \code{\link[base]{which}} }
  \item{negative}{ TRUE returns negative subscripts, FALSE returns positive subscripts, NA returns whatever requires less memory }
  \item{\dots}{ further arguments (formally required) }
}
\details{
  \code{as.logical.bit} and \code{as.integer.bit} return a vector of \code{FALSE, TRUE} resp. \code{0,1}.
  \code{which.bit} returns a vector of subscripts (\code{\link{which}}  has been made generic).
  Coercion from bit is quite fast because we use a double loop that fixes each word in a processor register.
}
\value{
  a vector of class 'logical' or 'integer'
}
\author{ Jens Oehlschlägel }
\seealso{ \code{\link{bit}}, \code{\link{as.bit}}, \code{\link{as.logical}}, \code{\link{as.integer}}, \code{\link{which}} }
\examples{
  x <- as.bit(c(FALSE, NA, TRUE, rep(TRUE, 9)))
  as.logical(x)
  as.integer(x)
  which.bit(x)
  which.bit(x, negative=TRUE)
  which.bit(x, negative=FALSE)
}
\keyword{ classes }
\keyword{ logic }
