% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bitsort.R
\name{bit_setops}
\alias{bit_setops}
\alias{bit_union}
\alias{bit_intersect}
\alias{bit_setdiff}
\alias{bit_symdiff}
\alias{bit_setequal}
\title{bit set operations}
\usage{
bit_union(x, y)

bit_intersect(x, y)

bit_setdiff(x, y)

bit_symdiff(x, y)

bit_setequal(x, y)
}
\arguments{
\item{x}{an integer vector}

\item{y}{an integer vector}
}
\value{
an integer vector
}
\description{
Fast versions of \code{\link{union}}, \code{\link{intersect}}, 
\code{\link{setdiff}}, symmetric difference and \code{\link{setequal}}
for integers.
}
\details{
determines the range of the integers and checks if the density justifies use 
of a bit vector; if yes, uses a bit vector for finding duplicates; if no, 
falls back to \code{\link{union}}, \code{\link{intersect}},
\code{\link{setdiff}}, \code{union(setdiff(x,y),setdiff(y,x))} and \code{\link{setequal}}
}
\section{Functions}{
\itemize{
\item \code{bit_union()}: union

\item \code{bit_intersect()}: intersection

\item \code{bit_setdiff()}: asymmetric difference

\item \code{bit_symdiff()}: symmetricx difference

\item \code{bit_setequal()}: equality

}}
\examples{
bit_union(1:2, 2:3)
bit_intersect(1:2, 2:3)
bit_setdiff(1:2, 2:3)
bit_symdiff(1:2, 2:3)
bit_setequal(1:2, 2:3)
bit_setequal(1:2, 2:1)
}
\seealso{
\code{\link{bit_in}}, \code{\link{bit_rangediff}}
}
