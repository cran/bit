\name{Summary}
\alias{all.bit}
\alias{any.bit}
\alias{min.bit}
\alias{max.bit}
\alias{range.bit}
\alias{sum.bit}
\alias{summary.bit}
\title{ Summaries of bit vectors }
\description{
  Fast aggregation functions for bit vectors.
}
\usage{
\method{all}{bit}(x, \dots)
\method{any}{bit}(x, \dots)
\method{min}{bit}(x, \dots)
\method{max}{bit}(x, \dots)
\method{range}{bit}(x, \dots)
\method{sum}{bit}(x, \dots)
\method{summary}{bit}(object, \dots)
}
\arguments{
  \item{x}{ an object of class bit, logical or integer }
  \item{object}{ an object of class bit }
  \item{\dots}{ formally required but not used }
}
\details{
  Bit summaries are quite fast because we use a double loop that fixes each word in a processor register.
  Furthermore we break out of looping as soon as possible.
}
\value{
  as expected
}
\author{ Jens Oehlschl�gel }
\seealso{ \code{\link{bit}}, \code{\link{all}}, \code{\link{any}}, \code{\link{min}}, \code{\link{max}}, \code{\link{range}}, \code{\link{sum}}, \code{\link{summary}} }
\examples{
  x <- as.bit(c(TRUE, TRUE))
  all(x)
  any(x)
  min(x)
  max(x)
  range(x)
  sum(x)
  summary(x)

 \dontrun{
    n <- .Machine$integer.max
    x <- !bit(n)
    N <- 1000000L  # batchsize
    B <- n %/% N   # number of batches
    R <- n %% N    # rest

    cat("Batched sum (52.5 sec on Centrino duo)\n")
    system.time({
      s <- 0L
      for (b in 1:B){
        s <- s + sum(x[((b-1L)*N+1L):(b*N)])
      }
      if (R)
        s <- s + sum(x[(n-R+1L):n])
    })

    cat("Batched sum saving repeated memory allocation for the return vector (44.4 sec on Centrino duo)\n")
    system.time({
      s <- 0L
      l <- logical(N)
      for (b in 1:B){
        .Call("R_bit_extract", x, ((b-1L)*N+1L):(b*N), l, PACKAGE = "bit")
        s <- s + sum(l)
      }
      if (R)
        s <- s + sum(x[(n-R+1L):n])
    })

    cat("C-coded sum (3.1 sec on Centrino duo)\n")
    system.time(sum(x))
 }
}
\keyword{ classes }
\keyword{ logic }