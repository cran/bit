# 1-bit boolean vectors for R
# Copyright 2008 Jens Oehlschlägel

# source("d:/mwp/eanalysis/bit/R/bit.R")

#! \name{bit-package}
#! \alias{bit-package}
#! \alias{bit}
#! \alias{print.bit}
#! \docType{package}
#! \title{
#!    A class for vectors of 1-bit booleans
#! }
#! \description{
#! Package 'bit' provides bitmapped vectors of booleans (no NAs),
#! coercion from and to logicals, integers and integer subscripts;
#! fast boolean operators and fast summary statistics. \cr
#!
#! With bit vectors you can store true binary booleans {FALSE,TRUE} at the expense
#! of 1 bit only, on a 32 bit architecture this means factor 32 less RAM and
#! factor 32 more speed on boolean operations. With this speed gain it even
#! pays-off to convert to bit in order to avoid a single boolean operation on
#! logicals or a single set operation on (longer) integer subscripts, the pay-off
#! is dramatic when such components are used more than once. \cr
#!
#! Reading from and writing to bit is approximately as fast as accessing standard
#! logicals - mostly due to R's time for memory allocation. The package allows to
#! work with pre-allocated memory for return values by calling .Call() directly:
#! when evaluating the speed of C-access with pre-allocated vector memory, coping
#! from bit to logical requires only 70% of the time for copying from logical to
#! logical; and copying from logical to bit comes at a performance penalty of 150%. \cr
#! }
#! \usage{
#!  bit(length)
#!  \method{print}{bit}(x, \dots)
#! }
#! \arguments{
#!   \item{length}{ length of vector in bits }
#!   \item{x}{ a bit vector }
#!   \item{\dots}{ further arguments to print }
#! }
#! \details{
#! \tabular{ll}{
#!    Package: \tab bit\cr
#!    Type: \tab Package\cr
#!    Version: \tab 1.0\cr
#!    Date: \tab 2008-09-13\cr
#!    License: \tab GPL-2\cr
#!    LazyLoad: \tab yes\cr
#!    Encoding: \tab latin1\cr
#!    Built: \tab R 2.7.2; i386-pc-mingw32; 2008-09-22 13:53:55; windows\cr
#! }
#!
#! Index:
#! \tabular{rrl}{
#!    \bold{function}               \tab \bold{see also}            \tab \bold{description} \cr
#!    \code{.BITS}                  \tab \code{\link{globalenv}}    \tab variable holding number of bits on this system \cr
#!    \code{\link{bit_init}}        \tab \code{\link{.First.lib}}   \tab initially allocate bit-masks (done in .First.lib) \cr
#!    \code{\link{bit_done}}        \tab \code{\link{.Last.lib}}    \tab finally de-allocate bit-masks (done in .Last.lib) \cr
#!    \code{\link{bit}}             \tab \code{\link{logical}}      \tab create bit vector \cr
#!    \code{\link{print.bit}}       \tab \code{\link{print}}        \tab print bit vector \cr
#!    \code{\link{length.bit}}      \tab \code{\link{length}}       \tab get length of bit vector \cr
#!    \code{\link{length<-.bit}}    \tab \code{\link{length<-}}     \tab change length of bit vector \cr
#!    \code{\link{is.bit}}          \tab \code{\link{is.logical}}   \tab test for bit vector \cr
#!    \code{\link{as.bit}}          \tab \code{\link{as.logical}}   \tab generically coerce to bit vector \cr
#!    \code{\link{as.bit.logical}}  \tab                            \tab coerce logical to bit vector (FALSE => FALSE, c(NA, TRUE) => TRUE) \cr
#!    \code{\link{as.bit.integer}}  \tab                            \tab coerce integer to bit vector (0 => FALSE, ELSE => TRUE) \cr
#!    \code{\link{as.bit.which}}    \tab                            \tab coerce integer subscripts to bit vector (TRUE at positive positions resp. FALSE at negative positions) \cr
#!    \code{\link{as.logical.bit}}  \tab \code{\link{as.logical}}   \tab coerce bit vector to logical (FALSE/TRUE) \cr
#!    \code{\link{as.integer.bit}}  \tab \code{\link{as.integer}}   \tab coerce bit vector to integer (0/1) \cr
#!    \code{\link{which.bit}}       \tab \code{\link[base]{which}}        \tab coerce bit vector to integer subscripts (positive or negative positions) \cr
#!    \code{\link{!.bit}}           \tab \code{\link{!}}            \tab boolean NOT \cr
#!    \code{\link{&.bit}}           \tab \code{\link{&}}            \tab boolean AND \cr
#!    \code{\link{|.bit}}           \tab \code{\link{|}}            \tab boolean OR \cr
#!    \code{\link{xor.bit}}         \tab \code{\link{xor}}          \tab boolean XOR \cr
#!    \code{\link{!=.bit}}          \tab \code{\link{!=}}           \tab boolean unequality (same as XOR) \cr
#!    \code{\link{==.bit}}          \tab \code{\link{==}}           \tab boolean equality \cr
#!    \code{\link{[[.bit}}          \tab \code{\link{[[}}           \tab get single bit (index checked) \cr
#!    \code{\link{[[<-.bit}}        \tab \code{\link{[[<-}}         \tab set single bit (index checked) \cr
#!    \code{\link{[.bit}}           \tab \code{\link{[}}            \tab get vector of bits (unchecked) \cr
#!    \code{\link{[<-.bit}}         \tab \code{\link{[<-}}          \tab set vector of bits (unchecked) \cr
#!    \code{\link{all.bit}}         \tab \code{\link{all}}          \tab aggregate AND \cr
#!    \code{\link{any.bit}}         \tab \code{\link{any}}          \tab aggregate OR \cr
#!    \code{\link{min.bit}}         \tab \code{\link{min}}          \tab aggregate MIN (integer version of ALL) \cr
#!    \code{\link{max.bit}}         \tab \code{\link{max}}          \tab aggregate MAX (integer version of ANY) \cr
#!    \code{\link{range.bit}}       \tab \code{\link{range}}        \tab aggregate [MIN,MAX] \cr
#!    \code{\link{sum.bit}}         \tab \code{\link{sum}}          \tab aggregate SUM (count of TRUE) \cr
#!    \code{\link{summary.bit}}     \tab \code{\link{tabulate}}     \tab aggregate count of FALSE and TRUE \cr
#!    \code{\link{regtest.bit}}     \tab                            \tab regressiontests for the package \cr
#!  }
#! }
#! \value{
#!   \code{bit} returns a vector of integer sufficiently long to store 'length' bits
#!   (but not longer) with an attribute 'n' and class 'bit'
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#!
#! Maintainer: Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \note{
#!   Since this package was created for high performance purposes, only positive integer subscripts are allowed.
#!   The '[' and '[<-' methods don't check whether the subscripts are positive integers in the allowed range.
#!   All R-functions behave as expected - i.e. they do not change their arguments and create new return values.
#!   If you want to save the time for return value memory allocation, you must use \code{\link{.Call}} directly
#!   (see the dontrun example in \code{\link{sum.bit}}).
#!   Note that the package has not been tested under 64 bit.
#!   Note also that the mapping of NAs to TRUE differs from the mapping of NAs to FALSE
#!   in \code{\link[ff]{vmode}="boolean"} in package ff (and one of the two may change in the future).
#! }
#! \keyword{ package }
#! \keyword{ classes }
#! \keyword{ logic }
#! \seealso{ \code{\link{logical}} in base R and \code{\link[ff]{vmode}} in package 'ff' }
#! \examples{
#!   x <- bit(12)                                 # create bit vector
#!   x                                            # autoprint bit vector
#!   length(x) <- 16                              # change length
#!   length(x)                                    # get length
#!   x[[2]]                                       # extract single element
#!   x[[2]] <- TRUE                               # replace single element
#!   x[1:2]                                       # extract parts of bit vector
#!   x[1:2] <- TRUE                               # replace parts of bit vector
#!   which(x)                                     # coerce bit to subscripts
#!   x <- as.bit.which(3:4, 4)                    # coerce subscripts to bit
#!   as.logical(x)                                # coerce bit to logical
#!   y <- as.bit(c(FALSE, TRUE, FALSE, TRUE))     # coerce logical to bit
#!   is.bit(y)                                    # test for bit
#!   !x                                           # boolean NOT
#!   x & y                                        # boolean AND
#!   x | y                                        # boolean OR
#!   xor(x, y)                                    # boolean Exclusive OR
#!   x != y                                       # boolean unequality (same as xor)
#!   x == y                                       # boolean equality
#!   all(x)                                       # aggregate AND
#!   any(x)                                       # aggregate OR
#!   min(x)                                       # aggregate MIN (integer version of ALL)
#!   max(x)                                       # aggregate MAX (integer version of ANY)
#!   range(x)                                     # aggregate [MIN,MAX]
#!   sum(x)                                       # aggregate SUM (count of TRUE)
#!   summary(x)                                   # aggregate count of FALSE and TRUE
#!
#!   \dontrun{
#!     cat("\nEven for a single boolean operation transforming logical to bit pays off\n")
#!     n <- 10000000
#!     x <- sample(c(FALSE, TRUE), n, TRUE)
#!     y <- sample(c(FALSE, TRUE), n, TRUE)
#!     system.time(x|y)
#!     system.time({
#!        x <- as.bit(x)
#!        y <- as.bit(y)
#!     })
#!     system.time( x | y )
#!     cat("Even more so if multiple operations are needed :-)\n")
#!
#!     cat("\nEven for a single set operation transforming subscripts to bit pays off\n")
#!     n <- 10000000
#!     x <- sample(n, n/2)
#!     y <- sample(n, n/2)
#!     system.time( union(x,y) )
#!     system.time({
#!      x <- as.bit.which(x, n)
#!      y <- as.bit.which(y, n)
#!     })
#!     system.time( which.bit( x | y ) )
#!     cat("Even more so if multiple operations are needed :-)\n")
#!
#!     cat("\nSome timings WITH memory allocation\n")
#!     n <- 2000000
#!     l <- sample(c(FALSE, TRUE), n, TRUE)
#!     # copy logical to logical
#!     system.time(for(i in 1:100){  # 0.0112
#!        l2 <- l
#!        l2[1] <- TRUE   # force new memory allocation (copy on modify)
#!        rm(l2)
#!     })/100
#!     # copy logical to bit
#!     system.time(for(i in 1:100){  # 0.0123
#!        b <- as.bit(l)
#!        rm(b)
#!     })/100
#!     # copy bit to logical
#!     b <- as.bit(l)
#!     system.time(for(i in 1:100){  # 0.009
#!        l2 <- as.logical(b)
#!        rm(l2)
#!     })/100
#!
#!     l2 <- l
#!     # replace logical by TRUE
#!     system.time(for(i in 1:100){
#!        l[] <- TRUE
#!     })/100
#!     # replace bit by TRUE (NOTE that we recycle the assignment value on R side == memory allocation and assignment first)
#!     system.time(for(i in 1:100){
#!        b[] <- TRUE
#!     })/100
#!     # THUS the following is faster
#!     system.time(for(i in 1:100){
#!        b <- !bit(n)
#!     })/100
#!
#!     # replace logical by logical
#!     system.time(for(i in 1:100){
#!        l[] <- l2
#!     })/100
#!     # replace bit by logical
#!     system.time(for(i in 1:100){
#!        b[] <- l2
#!     })/100
#!     # extract logical
#!     system.time(for(i in 1:100){
#!        l2[]
#!     })/100
#!     # extract bit
#!     system.time(for(i in 1:100){
#!        b[]
#!     })/100
#!
#!     cat("\nSome timings WITHOUT memory allocation (Serge, that's for you)\n")
#!     n <- 2000000
#!     l <- sample(c(FALSE, TRUE), n, TRUE)
#!     b <- as.bit(l)
#!     # read from logical, write to logical
#!     l2 <- logical(n)
#!     system.time(for(i in 1:100).Call("R_filter_getset", l, l2, PACKAGE="bit")) / 100
#!     # read from bit, write to logical
#!     l2 <- logical(n)
#!     system.time(for(i in 1:100).Call("R_bit_get", b, l2, PACKAGE="bit")) / 100
#!     # read from logical, write to bit
#!     system.time(for(i in 1:100).Call("R_bit_set", b, l2, PACKAGE="bit")) / 100
#!
#!   }
#! }

## 64-bit systems have 32-bit integers
##.BITS <- 8L * .Machine$sizeof.pointer
.BITS <-32


#! \name{bit_init}
#! \alias{bit_init}
#! \alias{bit_done}
#! \alias{.BITS}
#! \title{ Initializing bit masks }
#! \description{
#!   Functions to allocate (and de-allocate) bit masks
#! }
#! \usage{
#!   bit_init()
#!   bit_done()
#! }
#! \details{
#!   The C-code operates with bit masks.
#!   The memory for these is allocated dynamically.
#!   \code{bit_init} is called by \code{\link{.First.lib}}
#!   and \code{bit_done} is called by \code{\link{.Last.lib}}.
#!   You don't need to care about these under normal circumstances.
#! }
#! \value{
#!   NULL
#! }
#! \author{ Jens Oehlschlägel }
#! \seealso{ \code{\link{bit}}  }
#! \examples{
#!   bit_done()
#!   bit_init()
#! }
#! \keyword{ classes }
#! \keyword{ logic }



# initialize and finalize the bit-mask vectors used in C

bit_init <- function()
  .Call("R_bit_init", .BITS, PACKAGE="bit")

bit_done <- function()
  .Call("R_bit_done", PACKAGE="bit")



# creator for empty bit vector
bit <- function(length){
  length <- as.integer(length)
  if (length %% .BITS)
    n <- length %/% .BITS + 1L
  else
    n <- length %/% .BITS
  x <- integer(n)
  attr(x, "n") <- length
  class(x) <- "bit"
  x
}

print.bit <- function(x, ...){
  n <- length(x)
  cat("bit length=", n, " occupying only ", length(unclass(x)), " integers\n", sep="")
  if (n>16){
    y <- c(x[1:8], "..", x[(n-7L):n])
    names(y) <- c(1:8, "", (n-7L):n)
  }else{
    y <- c(x[])
    names(y) <- c(1:n)
  }
  print(y, quote=FALSE, ...)
}


#! \name{length.bit}
#! \alias{length.bit}
#! \alias{length<-.bit}
#! \title{ Getting and setting length of a bit vector }
#! \description{
#!   Query the number of bits in a bit vector or change the number of bits in a bit vector.
#! }
#! \usage{
#! \method{length}{bit}(x)
#! \method{length}{bit}(x) <- value
#! }
#! \arguments{
#!   \item{x}{ a bit vector }
#!   \item{value}{ the new number of bits }
#! }
#! \details{
#!   Note that no explicit initialization is done.
#!   As a consequence, when you first decrease and then increase length you might find that some 'new' bits are already \code{TRUE}.
#! }
#! \value{
#!   A bit vector with the new length
#! }
#! \author{ Jens Oehlschlägel }
#! \seealso{ \code{\link{bit}}, \code{\link{length}}  }
#! \examples{
#!   x <- bit(32)
#!   length(x)
#!   x[c(1, 32)] <- TRUE
#!   length(x) <- 16
#!   x
#!   length(x) <- 32
#!   x
#! }
#! \keyword{ classes }
#! \keyword{ logic }


length.bit <- function(x)
  attr(x, "n")

"length<-.bit" <- function(x, value){
  if (value!=attr(x, "n")){
    value <- as.integer(value)
    if (value %% .BITS)
      n <- value %/% .BITS + 1L
    else
      n <- value %/% .BITS
    cl <- class(x)
    x <- unclass(x)
    length(x) <- n
    attr(x, "n") <- value
    class(x) <- cl
    x
  }else
    x
}


#! \name{as.bit}
#! \alias{is.bit}
#! \alias{as.bit}
#! \alias{as.bit.bit}
#! \alias{as.bit.logical}
#! \alias{as.bit.integer}
#! \alias{as.bit.which}
#! \title{ Coercion to bit }
#! \description{
#!   Coerces logical or bit to bit vector (and test for bit)
#! }
#! \usage{
#! is.bit(x)
#! as.bit(x, \dots)
#! \method{as.bit}{bit}(x, \dots)
#! \method{as.bit}{logical}(x, \dots)
#! \method{as.bit}{integer}(x, \dots)
#! \method{as.bit}{which}(x, length, \dots)
#! }
#! \arguments{
#!   \item{x}{ an object of class bit, logical or integer }
#!   \item{length}{ the length of the new bit vector }
#!   \item{\dots}{ further arguments }
#! }
#! \details{
#!   Coercion to bit is quite fast because we use a double loop that fixes each word in a processor register
#! }
#! \note{
#!   Zero is coerced to FALSE, all other numbers including NA are coerced to TRUE.
#!   This differs from the NA-to-FALSE coercion in package ff and may change in the future.
#! }
#! \value{
#!   \code{is.bit} returns FALSE or TRUE, \code{as.bit} returns a vector of class 'bit'
#! }
#! \author{ Jens Oehlschlägel }
#! \seealso{ \code{\link{bit}}, \code{\link[bit:as.logical.bit]{as.logical}} }
#! \examples{
#!   x <- as.bit(c(FALSE, NA, TRUE))
#!   is.bit(x)
#!   as.bit(x)
#!   as.bit.which(c(1,3,4), 12)
#! }
#! \keyword{ classes }
#! \keyword{ logic }

is.bit <- function(x)
  inherits(x, "bit")

as.bit <- function(x, ...){
  UseMethod("as.bit", x)
}

as.bit.bit <- function(x, ...)
  x

as.bit.logical <- function(x, ...){
  b <- bit(length(x))
  .Call("R_bit_set", b, x, PACKAGE="bit")
}

as.bit.integer <- function(x, ...){
  b <- bit(length(x))
  .Call("R_bit_set_integer", b, x, PACKAGE="bit")
}

as.bit.which <- function(x, length, ...){
  b <- bit(length)
  if (length(x)){
    x <- as.integer(x)
    if (x[1]<0){
      b[-x] <- TRUE  # remember that negative indices are not allowed (and the assignment value is recycled to the length of the index)
      b <- !b
    }else{
      b[x] <- TRUE
    }
  }
  b
}


#! \name{as.logical.bit}
#! \alias{as.logical.bit}
#! \alias{as.integer.bit}
#! \alias{which}
#! \alias{which.default}
#! \alias{which.bit}
#! \title{ Coercion from bit }
#! \description{
#!   Coercing from bit to logical or integer
#! }
#! \usage{
#! \method{as.logical}{bit}(x, \dots)
#! \method{as.integer}{bit}(x, \dots)
#! which(x, \dots)
#! \method{which}{default}(x, arr.ind = FALSE, \dots)
#! \method{which}{bit}(x, negative = NA, \dots)
#! }
#! \arguments{
#!   \item{x}{ an object of class bit }
#!   \item{arr.ind}{ see \code{\link[base]{which}} }
#!   \item{negative}{ TRUE returns negative subscripts, FALSE returns positive subscripts, NA returns whatever requires less memory }
#!   \item{\dots}{ further arguments (formally required) }
#! }
#! \details{
#!   \code{as.logical.bit} and \code{as.integer.bit} return a vector of \code{FALSE, TRUE} resp. \code{0,1}.
#!   \code{which.bit} returns a vector of subscripts (\code{\link{which}}  has been made generic).
#!   Coercion from bit is quite fast because we use a double loop that fixes each word in a processor register.
#! }
#! \value{
#!   a vector of class 'logical' or 'integer'
#! }
#! \author{ Jens Oehlschlägel }
#! \seealso{ \code{\link{bit}}, \code{\link{as.bit}}, \code{\link{as.logical}}, \code{\link{as.integer}}, \code{\link{which}} }
#! \examples{
#!   x <- as.bit(c(FALSE, NA, TRUE, rep(TRUE, 9)))
#!   as.logical(x)
#!   as.integer(x)
#!   which.bit(x)
#!   which.bit(x, negative=TRUE)
#!   which.bit(x, negative=FALSE)
#! }
#! \keyword{ classes }
#! \keyword{ logic }


as.logical.bit <- function(x, ...){
  l <- logical(length(x))
  .Call("R_bit_get", x, l, PACKAGE="bit")
}

as.integer.bit <- function(x, ...){
  l <- integer(length(x))
  .Call("R_bit_get_integer", x, l, PACKAGE="bit")
}


if (!exists("which.default")){
  cat("creating generic 'which'\n")
  which.default <- get("which", mode = "function")
  formals(which.default)$"..." <- alist("..."=)[[1]]
  which <- function (x, ...)
    UseMethod("which")
}


which.bit <- function(x, negative=NA, ...){
  s <- sum(x)
  n <- length(x)
  if (is.na(negative)){
    if (s>(n%/%2L)){
      negative <- TRUE
      s <- n - s
    }else{
      negative <- FALSE
    }
  }else if(negative){
    s <- n - s
  }
  if (s)
    .Call("R_bit_which", x, s, negative, PACKAGE="bit")
  else
    integer()
}


#! \name{Logical}
#! \alias{!.bit}
#! \alias{&.bit}
#! \alias{|.bit}
#! \alias{==.bit}
#! \alias{!=.bit}
#! \alias{xor.bit}
#! \alias{xor.default}
#! \alias{xor}
#! \title{ Boolean operators and functions }
#! \description{
#!   Boolean negation, and, or and exclusive or.
#! }
#! \usage{
#! \method{!}{bit}(x)
#! \method{&}{bit}(e1, e2)
#! \method{|}{bit}(e1, e2)
#! \method{==}{bit}(e1, e2)
#! \method{!=}{bit}(e1, e2)
#! xor(x, y)
#! \method{xor}{default}(x, y)
#! \method{xor}{bit}(x, y)
#! }
#! \arguments{
#!   \item{x}{ a bit vector (or one logical vector in binary operators) }
#!   \item{y}{ a bit vector or an logical vector }
#!   \item{e1}{ a bit vector or an logical vector }
#!   \item{e2}{ a bit vector or an logical vector }
#! }
#! \details{
#!   Binary operators and function \code{xor} can combine 'bit' objects and 'logical' vectors.
#!   They do not recycle, thus the lengths of objects must match. Boolean operations on bit vectors are extremely fast
#!   because they are implemented using C's bitwise operators. If one argument is 'logical' it is converted to 'bit'. \cr
#!   The \code{xor} function has been made generic and \code{xor.default} has been implemented much faster than R's standard \code{\link[base]{xor}}.
#!   This was possible because actually boolean function \code{xor} and comparison operator \code{!=} do the same (even with NAs), and \code{!=} is much faster than the multiple calls in \code{(x | y) & !(x & y)}
#! }
#! \value{
#!   An object of class 'bit'
#! }
#! \author{ Jens Oehlschlägel }
#! \seealso{ \code{\link{bit}}, \code{\link{Logic}} }
#! \examples{
#!   x <- as.bit(c(FALSE, FALSE, FALSE, NA, NA, NA, TRUE, TRUE, TRUE))
#!   yl <- c(FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE)
#!   y <- as.bit(yl)
#!   !x
#!   x & y
#!   x | y
#!   xor(x, y)
#!   x != y
#!   x == y
#!   x & yl
#!   x | yl
#!   xor(x, yl)
#!   x != yl
#!   x == yl
#! }
#! \keyword{ classes }
#! \keyword{ logic }


"!.bit" <- function(x){
  if (length(x)){
    ret <- x
    ret[1] <- ret[1]  # force duplication
    .Call("R_bit_not", ret, PACKAGE="bit")
  }else{
    x
  }
}


"&.bit" <- function(e1, e2){
  if(length(e1)!=length(e2))
    stop("length(e1) != length(e2)")
  e1 <- as.bit(e1)
  e2 <- as.bit(e2)
  ret <- bit(length(e1))
  .Call("R_bit_and", e1, e2, ret, PACKAGE="bit")
}


"|.bit" <- function(e1, e2){
  if(length(e1)!=length(e2))
    stop("length(e1) != length(e2)")
  e1 <- as.bit(e1)
  e2 <- as.bit(e2)
  ret <- bit(length(e1))
  .Call("R_bit_or", e1, e2, ret, PACKAGE="bit")
}

xor <- function(x, y){
  UseMethod("xor", x)
}

xor.default <- function(x,y){
  as.logical(x) != as.logical(y)
}

"xor.bit" <- function(x, y){
  if(length(x)!=length(y))
    stop("length(x) != length(y)")
  x <- as.bit(x)
  y <- as.bit(y)
  ret <- bit(length(x))
  .Call("R_bit_xor", x, y, ret, PACKAGE="bit")
}

"!=.bit" <- function(e1, e2){
  if(length(e1)!=length(e2))
    stop("length(e1) != length(e2)")
  e1 <- as.bit(e1)
  e2 <- as.bit(e2)
  ret <- bit(length(e1))
  .Call("R_bit_xor", e1, e2, ret, PACKAGE="bit")
}

"==.bit" <- function(e1, e2){
  if(length(e1)!=length(e2))
    stop("length(e1) != length(e2)")
  e1 <- as.bit(e1)
  e2 <- as.bit(e2)
  ret <- bit(length(e1))
  .Call("R_bit_equal", e1, e2, ret, PACKAGE="bit")
}



#! \name{Summary}
#! \alias{all.bit}
#! \alias{any.bit}
#! \alias{min.bit}
#! \alias{max.bit}
#! \alias{range.bit}
#! \alias{sum.bit}
#! \alias{summary.bit}
#! \title{ Summaries of bit vectors }
#! \description{
#!   Fast aggregation functions for bit vectors.
#! }
#! \usage{
#! \method{all}{bit}(x, \dots)
#! \method{any}{bit}(x, \dots)
#! \method{min}{bit}(x, \dots)
#! \method{max}{bit}(x, \dots)
#! \method{range}{bit}(x, \dots)
#! \method{sum}{bit}(x, \dots)
#! \method{summary}{bit}(object, \dots)
#! }
#! \arguments{
#!   \item{x}{ an object of class bit, logical or integer }
#!   \item{object}{ an object of class bit }
#!   \item{\dots}{ formally required but not used }
#! }
#! \details{
#!   Bit summaries are quite fast because we use a double loop that fixes each word in a processor register.
#!   Furthermore we break out of looping as soon as possible.
#! }
#! \value{
#!   as expected
#! }
#! \author{ Jens Oehlschlägel }
#! \seealso{ \code{\link{bit}}, \code{\link{all}}, \code{\link{any}}, \code{\link{min}}, \code{\link{max}}, \code{\link{range}}, \code{\link{sum}}, \code{\link{summary}} }
#! \examples{
#!   x <- as.bit(c(TRUE, TRUE))
#!   all(x)
#!   any(x)
#!   min(x)
#!   max(x)
#!   range(x)
#!   sum(x)
#!   summary(x)
#!
#!  \dontrun{
#!     n <- .Machine$integer.max
#!     x <- !bit(n)
#!     N <- 1000000L  # batchsize
#!     B <- n %/% N   # number of batches
#!     R <- n %% N    # rest
#!
#!     cat("Batched sum (52.5 sec on Centrino duo)\n")
#!     system.time({
#!       s <- 0L
#!       for (b in 1:B){
#!         s <- s + sum(x[((b-1L)*N+1L):(b*N)])
#!       }
#!       if (R)
#!         s <- s + sum(x[(n-R+1L):n])
#!     })
#!
#!     cat("Batched sum saving repeated memory allocation for the return vector (44.4 sec on Centrino duo)\n")
#!     system.time({
#!       s <- 0L
#!       l <- logical(N)
#!       for (b in 1:B){
#!         .Call("R_bit_extract", x, ((b-1L)*N+1L):(b*N), l, PACKAGE = "bit")
#!         s <- s + sum(l)
#!       }
#!       if (R)
#!         s <- s + sum(x[(n-R+1L):n])
#!     })
#!
#!     cat("C-coded sum (3.1 sec on Centrino duo)\n")
#!     system.time(sum(x))
#!  }
#! }
#! \keyword{ classes }
#! \keyword{ logic }


sum.bit <- function(x, ...){
  .Call("R_bit_sum", x, PACKAGE="bit")
}

all.bit <- function(x, ...){
  .Call("R_bit_all", x, PACKAGE="bit")
}

any.bit <- function(x, ...){
  .Call("R_bit_any", x, PACKAGE="bit")
}

min.bit <- function(x, ...){
  as.integer(.Call("R_bit_all", x, PACKAGE="bit"))
}

max.bit <- function(x, ...){
  as.integer(.Call("R_bit_any", x, PACKAGE="bit"))
}

range.bit <- function(x, ...){
  as.integer(c(.Call("R_bit_all", x, PACKAGE="bit"), .Call("R_bit_any", x, PACKAGE="bit")))
}

summary.bit <- function(object, ...){
  n <- length(object)
  s <- sum(object)
  c("FALSE"=n-s, "TRUE"=s)
}



#! \name{Extract}
#! \alias{[[.bit}
#! \alias{[[<-.bit}
#! \alias{[.bit}
#! \alias{[<-.bit}
#! \title{ Extract or replace part of an bit vector }
#! \description{
#!   Operators acting on bit objects to extract or replace parts.
#! }
#! \usage{
#! \method{[[}{bit}(x, i)
#! \method{[[}{bit}(x, i) <- value
#! \method{[}{bit}(x, i)
#! \method{[}{bit}(x, i) <- value
#! }
#! \arguments{
#!   \item{x}{ a bit object }
#!   \item{i}{ positive integer subscript }
#!   \item{value}{ new logical or integer values }
#! }
#! \details{
#!   Since this package was created for high performance purposes, only positive integer subscripts are allowed.
#!   The '[' and '[<-' methods don't check whether the subscripts are positive integers in the allowed range.
#! }
#! \value{
#!   The extractors \code{[[} and \code{[} return a logical scalar or vector.
#!   The replacment functions return a bit object.
#! }
#! \author{ Jens Oehlschlägel }
#! \seealso{ \code{\link{bit}}, \code{\link{Extract}} }
#! \examples{
#!   x <- as.bit(c(FALSE, NA, TRUE))
#!   x[] <- c(FALSE, NA, TRUE)
#!   x[1:2]
#!   x[[1]]
#!   x[] <- TRUE
#!   x[1:2] <- FALSE
#!   x[[1]] <- TRUE
#! }
#! \keyword{ classes }
#! \keyword{ logic }



"[[.bit" <- function(x, i){
  if (length(i)!=1)
    stop("subscript length not 1")
  if (is.numeric(i)){
    i <- as.integer(i)
    if (is.na(i) || i<1L || i>length(x))
      stop("subscript must be positive integer (or double) within length")
    ret <- logical(1L)
    .Call("R_bit_extract", x, i, ret, PACKAGE="bit")
  }else
    stop("subscript must be positive integer (or double) within length")
}


"[[<-.bit" <- function(x, i, value){
  if (length(i)!=1)
    stop("subscript length not 1")
  if (length(value)!=1)
    stop("value length not 1")
  if (is.numeric(i)){
    i <- as.integer(i)
    if (is.na(i) || i<1L || i>length(x))
      stop("subscript must be positive integer (or double) within length")
    value2 <- as.logical(value)
    .Call("R_bit_replace", x, i, value2, PACKAGE="bit")
  }else
    stop("subscript must be positive integer (or double) within length")
}


"[.bit" <- function(x, i){
  if (missing(i) || (n <- length(i))==0L){
    ret <- logical(length(x))
    .Call("R_bit_get", x, ret, PACKAGE="bit")
  }else{
    if (is.numeric(i)){
      i <- as.integer(i)
      N <- length(x)
      if (negative <- i[1]<0){
        ret <- logical(N-n)
        .Call("R_bit_extract", x, i, ret, negative, PACKAGE="bit")
      }else{
        ret <- logical(n)
        .Call("R_bit_extract", x, i, ret, negative, PACKAGE="bit")
      }
    }else
      stop("subscript must be integer (or double) within length")
  }
}


"[<-.bit" <- function(x, i, value){
  if (missing(i) || (n <- length(i))==0L){
    if (length(value)==length(x)){
      value2 <- as.logical(value)
    }else{
      value2 <- logical(length(x))
      value2[] <- value
    }
    .Call("R_bit_set", x, value2, PACKAGE="bit")
  }else{
    if (is.numeric(i)){
      i <- as.integer(i)
      N <- length(x)
      if (negative <- i[1]<0)
        n <- N - n
      if (length(value)==n){
        value2 <- as.logical(value)
      }else{
        value2 <- logical(length(i))
        value2[] <- value
      }
      ret <- logical(n)
      .Call("R_bit_replace", x, i, value2, negative, PACKAGE="bit")
    }else
      stop("subscript must be integer (or double) within length")
  }
}


#! \name{regtest.bit}
#! \alias{regtest.bit}
#! \title{ Regressiontests for bit }
#! \description{
#!   Test package bit for correctness
#! }
#! \usage{
#! regtest.bit(N = 100)
#! }
#! \arguments{
#!   \item{N}{ number of random test runs }
#! }
#! \details{
#!   random data of random length are generated and correctness of package functions tested on these
#! }
#! \value{
#!   a vector of class 'logical' or 'integer'
#! }
#! \author{ Jens Oehlschlägel }
#! \seealso{ \code{\link{bit}}, \code{\link{as.bit}}, \code{\link{as.logical}}, \code{\link{as.integer}}, \code{\link{which}} }
#! \examples{
#!   if (regtest.bit()){
#!     cat("regtest.bit is OK\n")
#!   }else{
#!     stop("regtest.bit failed")
#!   }
#!
#!   \dontrun{
#!     regtest.bit(10000)
#!   }
#! }
#! \keyword{ classes }
#! \keyword{ logic }

regtest.bit <- function(
    N = 100  # number of repetitions for random regression tests
)
{
  OK <- TRUE
  pool <- c(FALSE, TRUE)

  for (i in 1:N){
    n <- sample(1:(2*.BITS), 1)
    l <- sample(pool, n, TRUE)
    # check direct coercion
    b <- as.bit(l)
    l2 <- as.logical(b)
    if (!identical(l,l2)){
      cat("\nregression test difference between logical\n")
      print(l)
      cat("and as.logical(as.bit(logical))\n")
      print(l2)
      OK <- FALSE
    }
    # summary functions with logical return
    s <- c(all=all(l), any=any(l))
    s2 <- c(all=all(b), any=any(b))
    if (!identical(s,s2)){
      cat("\nregression test difference between logical summaries\n")
      print(s)
      cat("and bit summaries\n")
      print(s2)
      OK <- FALSE
    }
    # summary functions with integer return
    s <- c(min=min(l), max=max(l), range=range(l), sum=sum(l), tabulate=tabulate(1+l, 2))
    s2 <- c(min=min(b), max=max(b), range=range(b), sum=sum(b), tabulate=as.vector(summary(b)))
    if (!identical(s,s2)){
      cat("\nregression test difference between logical summaries\n")
      print(s)
      cat("and bit summaries\n")
      print(s2)
      OK <- FALSE
    }
    # check positive whichs
    w <- which(l)
    w2 <- which(as.bit.which(w, n), negative=FALSE)
    if (!identical(w,w2)){
      cat("\nregression test difference between which\n")
      print(w)
      cat("and which(as.bit.which(which))\n")
      print(w2)
      OK <- FALSE
    }
    # check automatic whichs (pos or neg whatever shorter)
    if (sum(l)>(n%/%2L))
      w <- -which(!l)
    else
      w <- which(l)
    w2 <- which(as.bit.which(w, n))
    if (!identical(w,w2)){
      cat("\nregression test difference between which\n")
      print(w)
      cat("and which(as.bit.which(which))\n")
      print(w2)
      OK <- FALSE
    }
    # check boolean operators
    l2 <- sample(c(FALSE, TRUE), n, TRUE)
    b2 <- as.bit(l2)
    ops <- c(
      NOT = identical(!l, as.logical(!b))
    , AND = identical(l&l2, as.logical(b&b2))
    , OR = identical(l|l2, as.logical(b|b2))
    , XOR = identical(xor(l,l2), as.logical(xor(b,b2)))
    , NEQ = identical(l!=l2, as.logical(b!=b2))
    , EQ = identical(l==l2, as.logical(b==b2))
    )
    if (!all(ops)){
      cat("\nregression test difference for boolean operators(s)\n")
      print(ops)
      print(cbind(l=l, l2=l))
      OK <- FALSE
    }
    rm(l2,b2)
    # check extractors
    n2 <- sample(1:n, 1)
    j <- sample(1:n, n2)
    if (!identical(l[j], b[j])){
      cat("\nregression test difference when extracting\n")
      OK <- FALSE
    }
    # check replacement (index)
    new <- sample(pool, n2, TRUE)
    l[j] <- new
    b[j] <- new
    if (!identical(l, b[])){
      cat("\nregression test difference when replacing with index\n")
      OK <- FALSE
    }
    # check replacement (recycle)
    if (n%%2){
      new <- sample(pool, 1)
      l[] <- new
      b[] <- new
    }else{
      l[] <- pool
      b[] <- pool
    }
    if (!identical(l, as.logical(b))){
      cat("\nregression test difference when replacing with recylcling\n")
      OK <- FALSE
    }
  }

  N <- 2L*.BITS
  l <- logical(N)
  b <- bit(N)
  for (i in 1:N){
    l[i] <- TRUE
    b[i] <- TRUE
    if (!identical(l,as.logical(b))){
      cat("\nregression test difference when replacing at position", i, "\n")
      OK <- FALSE
    }
  }

  OK
}
