% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bit.R, R/generics.R
\name{as.which.which}
\alias{as.which.which}
\alias{as.which.NULL}
\alias{as.which.numeric}
\alias{as.which.integer}
\alias{as.which.logical}
\alias{as.which.ri}
\alias{as.which.bit}
\alias{as.which.bitwhich}
\alias{as.which}
\title{Coercion to (positive) integer positions}
\usage{
\method{as.which}{which}(x, maxindex = NA_integer_, ...)

\method{as.which}{`NULL`}(x, ...)

\method{as.which}{numeric}(x, maxindex = NA_integer_, ...)

\method{as.which}{integer}(x, maxindex = NA_integer_, is.unsorted = TRUE, has.dup = TRUE, ...)

\method{as.which}{logical}(x, ...)

\method{as.which}{ri}(x, ...)

\method{as.which}{bit}(x, range = NULL, ...)

\method{as.which}{bitwhich}(x, ...)

as.which(x, ...)
}
\arguments{
\item{x}{an object of classes \code{\link{bit}}, \code{\link{bitwhich}},
\code{\link{ri}} or something on which \code{\link{which}} works}

\item{maxindex}{the length of the boolean vector which is represented}

\item{\dots}{further arguments (passed to \code{\link{which}} for the
default method, ignored otherwise)}

\item{is.unsorted}{a logical scalar indicating whether the data may be unsorted}

\item{has.dup}{a logical scalar indicating whether the data may have duplicates}

\item{range}{a \code{\link{ri}} or an integer vector of length==2 giving a
range restriction for chunked processing}
}
\value{
a vector of class 'logical' or 'integer'
}
\description{
Coercing to something like the result of which \code{\link{which}}
}
\details{
\code{as.which.bit} returns a vector of subscripts with class 'which'
}
\section{Methods (by class)}{
\itemize{
\item \code{which}: method to coerce to \code{\link[=as.which]{which}} from \code{\link[=as.which]{which}}

\item \code{NULL}: method to coerce to zero length \code{\link[=as.which]{which}} from \code{\link{NULL}}

\item \code{numeric}: method to coerce to \code{\link[=as.which]{which}} from \code{\link{numeric}}

\item \code{integer}: method to coerce to \code{\link[=as.which]{which}} from \code{\link{integer}}

\item \code{logical}: method to coerce to \code{\link[=as.which]{which}} from \code{\link{logical}}

\item \code{ri}: method to coerce to \code{\link[=as.which]{which}} from \code{\link{ri}}

\item \code{bit}: method to coerce to \code{\link[=as.which]{which}} from \code{\link{bit}}

\item \code{bitwhich}: method to coerce to \code{\link[=as.which]{which}} from \code{\link{bitwhich}}
}}

\examples{

  r <- ri(5, 20, 100)
  x <- as.which(r)
  x

  stopifnot(identical(x, as.which(as.logical(r))))
  stopifnot(identical(x, as.which(as.bitwhich(r))))
  stopifnot(identical(x, as.which(as.bit(r))))

}
\seealso{
\code{\link{CoercionToStandard}}, \code{\link{as.booltype}}, \code{\link{as.bit}}, \code{\link{as.bitwhich}}
, \code{\link{as.which}}, \code{\link{as.ri}}, \code{\link[ff]{as.hi}},  \code{\link[ff]{as.ff}}
}
\author{
Jens Oehlschlägel
}
\keyword{classes}
\keyword{logic}
