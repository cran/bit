\name{Logical}
\alias{!.bit}
\alias{&.bit}
\alias{|.bit}
\alias{==.bit}
\alias{!=.bit}
\alias{xor.bit}
\alias{xor.default}
\alias{xor}
\title{ Boolean operators and functions }
\description{
  Boolean negation, and, or and exclusive or.
}
\usage{
\method{!}{bit}(x)
\method{&}{bit}(e1, e2)
\method{|}{bit}(e1, e2)
\method{==}{bit}(e1, e2)
\method{!=}{bit}(e1, e2)
xor(x, y)
\method{xor}{default}(x, y)
\method{xor}{bit}(x, y)
}
\arguments{
  \item{x}{ a bit vector (or one logical vector in binary operators) }
  \item{y}{ a bit vector or an logical vector }
  \item{e1}{ a bit vector or an logical vector }
  \item{e2}{ a bit vector or an logical vector }
}
\details{
  Binary operators and function \code{xor} can combine 'bit' objects and 'logical' vectors.
  They do not recycle, thus the lengths of objects must match. Boolean operations on bit vectors are extremely fast
  because they are implemented using C's bitwise operators. If one argument is 'logical' it is converted to 'bit'. \cr
  The \code{xor} function has been made generic and \code{xor.default} has been implemented much faster than R's standard \code{\link[base]{xor}}.
  This was possible because actually boolean function \code{xor} and comparison operator \code{!=} do the same (even with NAs), and \code{!=} is much faster than the multiple calls in \code{(x | y) & !(x & y)}
}
\value{
  An object of class 'bit'
}
\author{ Jens Oehlschlägel }
\seealso{ \code{\link{bit}}, \code{\link{Logic}} }
\examples{
  x <- as.bit(c(FALSE, FALSE, FALSE, NA, NA, NA, TRUE, TRUE, TRUE))
  yl <- c(FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE)
  y <- as.bit(yl)
  !x
  x & y
  x | y
  xor(x, y)
  x != y
  x == y
  x & yl
  x | yl
  xor(x, yl)
  x != yl
  x == yl
}
\keyword{ classes }
\keyword{ logic }
