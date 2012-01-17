pkgname <- "bit"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('bit')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Extract")
### * Extract

flush(stderr()); flush(stdout())

### Name: Extract
### Title: Extract or replace part of an bit vector
### Aliases: [[.bit [[<-.bit [.bit [<-.bit
### Keywords: classes logic

### ** Examples

  x <- as.bit(c(FALSE, NA, TRUE))
  x[] <- c(FALSE, NA, TRUE)
  x[1:2]
  x[-3]
  x[ri(1,2)]
  x[as.bitwhich(c(TRUE,TRUE,FALSE))]
  x[[1]]
  x[] <- TRUE
  x[1:2] <- FALSE
  x[[1]] <- TRUE



cleanEx()
nameEx("LogicBit")
### * LogicBit

flush(stderr()); flush(stdout())

### Name: LogicBit
### Title: Boolean operators and functions for class bit
### Aliases: LogicBit !.bit !.bitwhich &.bit &.bitwhich |.bit |.bitwhich
###   ==.bit ==.bitwhich !=.bit !=.bitwhich xor xor.default xor.bit
###   xor.bitwhich
### Keywords: classes logic

### ** Examples

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

  x <- as.bitwhich(c(FALSE, FALSE, FALSE, NA, NA, NA, TRUE, TRUE, TRUE))
  yl <- c(FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE)
  y <- as.bitwhich(yl)
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



cleanEx()
nameEx("Summary")
### * Summary

flush(stderr()); flush(stdout())

### Name: Summary
### Title: Summaries of bit vectors
### Aliases: all.bit any.bit min.bit max.bit range.bit sum.bit summary.bit
###   all.bitwhich any.bitwhich min.bitwhich max.bitwhich range.bitwhich
###   sum.bitwhich summary.bitwhich all.ri any.ri min.ri max.ri range.ri
###   sum.ri summary.ri
### Keywords: classes logic

### ** Examples

  x <- as.bit(c(TRUE, TRUE))
  all(x)
  any(x)
  min(x)
  max(x)
  range(x)
  sum(x)
  summary(x)

  x <- as.bitwhich(c(TRUE, TRUE))
  all(x)
  any(x)
  min(x)
  max(x)
  range(x)
  sum(x)
  summary(x)

 ## Not run: 
##D     n <- .Machine$integer.max
##D     x <- !bit(n)
##D     N <- 1000000L  # batchsize
##D     B <- n %/% N   # number of batches
##D     R <- n %% N    # rest
##D 
##D     message("Batched sum (52.5 sec on Centrino duo)")
##D     system.time({
##D       s <- 0L
##D       for (b in 1:B){
##D         s <- s + sum(x[((b-1L)*N+1L):(b*N)])
##D       }
##D       if (R)
##D         s <- s + sum(x[(n-R+1L):n])
##D     })
##D 
##D     message("Batched sum saving repeated memory allocation for the return vector (44.4 sec on Centrino duo)")
##D     system.time({
##D       s <- 0L
##D       l <- logical(N)
##D       for (b in 1:B){
##D         .Call("R_bit_extract", x, ((b-1L)*N+1L):(b*N), l, PACKAGE = "bit")
##D         s <- s + sum(l)
##D       }
##D       if (R)
##D         s <- s + sum(x[(n-R+1L):n])
##D     })
##D 
##D     message("C-coded sum (3.1 sec on Centrino duo)")
##D     system.time(sum(x))
##D  
## End(Not run)



cleanEx()
nameEx("as.bit")
### * as.bit

flush(stderr()); flush(stdout())

### Name: as.bit
### Title: Coercing to bit
### Aliases: as.bit as.bit.bit as.bit.logical as.bit.integer as.bit.double
###   as.bit.bitwhich as.bit.which as.bit.ri
### Keywords: classes logic

### ** Examples

  x <- as.bit(c(FALSE, NA, TRUE))
  as.bit(x)
  as.bit.which(c(1,3,4), 12)



cleanEx()
nameEx("as.bitwhich")
### * as.bitwhich

flush(stderr()); flush(stdout())

### Name: as.bitwhich
### Title: Coercing to bitwhich
### Aliases: as.bitwhich as.bitwhich.bit as.bitwhich.bitwhich
###   as.bitwhich.ri as.bitwhich.which as.bitwhich.integer
###   as.bitwhich.double as.bitwhich.logical
### Keywords: classes logic

### ** Examples

 as.bitwhich(c(FALSE, FALSE, FALSE))
 as.bitwhich(c(FALSE, FALSE, TRUE))
 as.bitwhich(c(FALSE, TRUE, TRUE))
 as.bitwhich(c(TRUE, TRUE, TRUE))



cleanEx()
nameEx("as.logical.bit")
### * as.logical.bit

flush(stderr()); flush(stdout())

### Name: as.logical.bit
### Title: Coercion from bit, bitwhich and ri to logical, integer, double
### Aliases: as.logical.bit as.integer.bit as.double.bit
###   as.logical.bitwhich as.integer.bitwhich as.double.bitwhich
###   as.logical.ri as.integer.ri as.double.ri
### Keywords: classes logic

### ** Examples

  x <- ri(2, 5, 10)
  y <- as.logical(x)
  y
  stopifnot(identical(y, as.logical(as.bit(x))))
  stopifnot(identical(y, as.logical(as.bitwhich(x))))

  y <- as.integer(x)
  y
  stopifnot(identical(y, as.integer(as.logical(x))))
  stopifnot(identical(y, as.integer(as.bit(x))))
  stopifnot(identical(y, as.integer(as.bitwhich(x))))

  y <- as.double(x)
  y
  stopifnot(identical(y, as.double(as.logical(x))))
  stopifnot(identical(y, as.double(as.bit(x))))
  stopifnot(identical(y, as.double(as.bitwhich(x))))



cleanEx()
nameEx("as.which")
### * as.which

flush(stderr()); flush(stdout())

### Name: as.which
### Title: Coercion to (positive) integer positions
### Aliases: as.which as.which.default as.which.bitwhich as.which.bit
###   as.which.ri
### Keywords: classes logic

### ** Examples

  r <- ri(5, 20, 100)
  x <- as.which(r)
  x

  stopifnot(identical(x, as.which(as.logical(r))))
  stopifnot(identical(x, as.which(as.bitwhich(r))))
  stopifnot(identical(x, as.which(as.bit(r))))



cleanEx()
nameEx("bbatch")
### * bbatch

flush(stderr()); flush(stdout())

### Name: bbatch
### Title: Balanced Batch sizes
### Aliases: bbatch
### Keywords: IO data

### ** Examples

  bbatch(100, 24)



cleanEx()
nameEx("bit-package")
### * bit-package

flush(stderr()); flush(stdout())

### Name: bit-package
### Title: A class for vectors of 1-bit booleans
### Aliases: bit-package bit print.bit
### Keywords: package classes logic

### ** Examples

  x <- bit(12)                                 # create bit vector
  x                                            # autoprint bit vector
  length(x) <- 16                              # change length
  length(x)                                    # get length
  x[[2]]                                       # extract single element
  x[[2]] <- TRUE                               # replace single element
  x[1:2]                                       # extract parts of bit vector
  x[1:2] <- TRUE                               # replace parts of bit vector
  as.which(x)                                  # coerce bit to subscripts
  x <- as.bit.which(3:4, 4)                    # coerce subscripts to bit
  as.logical(x)                                # coerce bit to logical
  y <- as.bit(c(FALSE, TRUE, FALSE, TRUE))     # coerce logical to bit
  is.bit(y)                                    # test for bit
  !x                                           # boolean NOT
  x & y                                        # boolean AND
  x | y                                        # boolean OR
  xor(x, y)                                    # boolean Exclusive OR
  x != y                                       # boolean unequality (same as xor)
  x == y                                       # boolean equality
  all(x)                                       # aggregate AND
  any(x)                                       # aggregate OR
  min(x)                                       # aggregate MIN (integer version of ALL)
  max(x)                                       # aggregate MAX (integer version of ANY)
  range(x)                                     # aggregate [MIN,MAX]
  sum(x)                                       # aggregate SUM (count of TRUE)
  summary(x)                                   # aggregate count of FALSE and TRUE

  ## Not run: 
##D     message("\nEven for a single boolean operation transforming logical to bit pays off")
##D     n <- 10000000
##D     x <- sample(c(FALSE, TRUE), n, TRUE)
##D     y <- sample(c(FALSE, TRUE), n, TRUE)
##D     system.time(x|y)
##D     system.time({
##D        x <- as.bit(x)
##D        y <- as.bit(y)
##D     })
##D     system.time( z <- x | y )
##D     system.time( as.logical(z) )
##D     message("Even more so if multiple operations are needed :-)")
##D 
##D     message("\nEven for a single set operation transforming subscripts to bit pays off\n")
##D     n <- 10000000
##D     x <- sample(n, n/2)
##D     y <- sample(n, n/2)
##D     system.time( union(x,y) )
##D     system.time({
##D      x <- as.bit.which(x, n)
##D      y <- as.bit.which(y, n)
##D     })
##D     system.time( as.which.bit( x | y ) )
##D     message("Even more so if multiple operations are needed :-)")
##D 
##D     message("\nSome timings WITH memory allocation")
##D     n <- 2000000
##D     l <- sample(c(FALSE, TRUE), n, TRUE)
##D     # copy logical to logical
##D     system.time(for(i in 1:100){  # 0.0112
##D        l2 <- l
##D        l2[1] <- TRUE   # force new memory allocation (copy on modify)
##D        rm(l2)
##D     })/100
##D     # copy logical to bit
##D     system.time(for(i in 1:100){  # 0.0123
##D        b <- as.bit(l)
##D        rm(b)
##D     })/100
##D     # copy bit to logical
##D     b <- as.bit(l)
##D     system.time(for(i in 1:100){  # 0.009
##D        l2 <- as.logical(b)
##D        rm(l2)
##D     })/100
##D     # copy bit to bit
##D     b <- as.bit(l)
##D     system.time(for(i in 1:100){  # 0.009
##D        b2 <- b
##D        b2[1] <- TRUE   # force new memory allocation (copy on modify)
##D        rm(b2)
##D     })/100
##D 
##D 
##D     l2 <- l
##D     # replace logical by TRUE
##D     system.time(for(i in 1:100){
##D        l[] <- TRUE
##D     })/100
##D     # replace bit by TRUE (NOTE that we recycle the assignment value on R side == memory allocation and assignment first)
##D     system.time(for(i in 1:100){
##D        b[] <- TRUE
##D     })/100
##D     # THUS the following is faster
##D     system.time(for(i in 1:100){
##D        b <- !bit(n)
##D     })/100
##D 
##D     # replace logical by logical
##D     system.time(for(i in 1:100){
##D        l[] <- l2
##D     })/100
##D     # replace bit by logical
##D     system.time(for(i in 1:100){
##D        b[] <- l2
##D     })/100
##D     # extract logical
##D     system.time(for(i in 1:100){
##D        l2[]
##D     })/100
##D     # extract bit
##D     system.time(for(i in 1:100){
##D        b[]
##D     })/100
##D 
##D     message("\nSome timings WITHOUT memory allocation (Serge, that's for you)")
##D     n <- 2000000L
##D     l <- sample(c(FALSE, TRUE), n, TRUE)
##D     b <- as.bit(l)
##D     # read from logical, write to logical
##D     l2 <- logical(n)
##D     system.time(for(i in 1:100).Call("R_filter_getset", l, l2, PACKAGE="bit")) / 100
##D     # read from bit, write to logical
##D     l2 <- logical(n)
##D     system.time(for(i in 1:100).Call("R_bit_get", b, l2, c(1L, n), PACKAGE="bit")) / 100
##D     # read from logical, write to bit
##D     system.time(for(i in 1:100).Call("R_bit_set", b, l2, c(1L, n), PACKAGE="bit")) / 100
##D 
##D   
## End(Not run)



cleanEx()
nameEx("bit_init")
### * bit_init

flush(stderr()); flush(stdout())

### Name: bit_init
### Title: Initializing bit masks
### Aliases: bit_init bit_done .BITS
### Keywords: classes logic

### ** Examples

  bit_done()
  bit_init()



cleanEx()
nameEx("bitwhich")
### * bitwhich

flush(stderr()); flush(stdout())

### Name: bitwhich
### Title: A class for vectors representing asymetric selections
### Aliases: bitwhich print.bitwhich
### Keywords: classes logic

### ** Examples

 bitwhich(12, x=c(1,3), poslength=2)
 bitwhich(12, x=-c(1,3), poslength=10)



cleanEx()
nameEx("c.bit")
### * c.bit

flush(stderr()); flush(stdout())

### Name: c.bit
### Title: Concatenating bit and bitwhich vectors
### Aliases: c.bit c.bitwhich
### Keywords: classes logic

### ** Examples

 c(bit(4), bit(4))



cleanEx()
nameEx("chunk")
### * chunk

flush(stderr()); flush(stdout())

### Name: chunk
### Title: Chunked range index
### Aliases: chunk chunk.default
### Keywords: data

### ** Examples

  chunk(1, 100, by=30)
  chunk(1, 100, by=30, method="seq")
   ## Not run: 
##D       require(foreach)
##D       m <- 10000
##D       k <- 1000
##D       n <- m*k
##D       message("Four ways to loop from 1 to n. Slowest foreach to fastest chunk is 1700:1 on a dual core notebook with 3GB RAM\n")
##D       z <- 0L; print(k*system.time({it <- icount(m); foreach (i = it) %do% { z <- i; NULL }})); z
##D       z <- 0L; print(system.time({i <- 0L;while (i<n) {i <- i + 1L; z <- i}})); z
##D       z <- 0L; print(system.time(for (i in 1:n) z <- i)); z
##D       z <- 0L; n <- m*k; print(system.time(for (ch in chunk(1, n, by=m)){for (i in ch[1]:ch[2])z <- i})); z
##D       message("Seven ways to calculate sum(1:n). Slowest foreach to fastest chunk is 61000:1 on a dual core notebook with 3GB RAM\n")
##D       print(k*system.time({it <- icount(m); foreach (i = it, .combine="+") %do% { i }}))
##D       z <- 0; print(k*system.time({it <- icount(m); foreach (i = it) %do% { z <- z + i; NULL }})); z
##D       z <- 0; print(system.time({i <- 0L;while (i<n) {i <- i + 1L; z <- z + i}})); z
##D       z <- 0; print(system.time(for (i in 1:n) z <- z + i)); z
##D       print(system.time(sum(as.double(1:n))))
##D       z <- 0; n <- m*k; print(system.time(for (ch in chunk(1, n, by=m)){for (i in ch[1]:ch[2])z <- z + i})); z
##D       z <- 0; n <- m*k; print(system.time(for (ch in chunk(1, n, by=m)){z <- z + sum(as.double(ch[1]:ch[2]))})); z
##D    
## End(Not run)



cleanEx()
nameEx("intrle")
### * intrle

flush(stderr()); flush(stdout())

### Name: intrle
### Title: Hybrid Index, C-coded utilities
### Aliases: intrle intisasc intisdesc
### Keywords: IO data

### ** Examples

  intrle(sample(1:100))
  intrle(diff(1:100))
  intisasc(1:100)
  intisasc(100:1)
  intisasc(c(NA, 1:100))
  intisdesc(1:100)
  intisdesc(100:1)



cleanEx()
nameEx("is.bit")
### * is.bit

flush(stderr()); flush(stdout())

### Name: is.bit
### Title: Testing for bit, bitwhich and ri selection classes
### Aliases: is.ri is.bit is.bitwhich
### Keywords: classes logic

### ** Examples

 is.ri(TRUE)
 is.ri(ri(1,4,12))
 is.bit(TRUE)
 is.bitwhich(TRUE)
 is.bit(as.bit(TRUE))
 is.bitwhich(as.bitwhich(TRUE))



cleanEx()
nameEx("is.sorted")
### * is.sorted

flush(stderr()); flush(stdout())

### Name: is.sorted
### Title: Generics related to cache access
### Aliases: is.sorted na.count nvalid nunique nties is.sorted<- na.count<-
###   nunique<- nties<-
### Keywords: environment methods

### ** Examples

	methods("na.count")



cleanEx()
nameEx("length.bit")
### * length.bit

flush(stderr()); flush(stdout())

### Name: length.bit
### Title: Getting and setting length of bit, bitwhich and ri objects
### Aliases: length.bit length.bitwhich length.ri length<-.bit
###   length<-.bitwhich
### Keywords: classes logic

### ** Examples

  stopifnot(length(ri(1, 1, 32))==32)

  x <- as.bit(ri(32, 32, 32))
  stopifnot(length(x)==32)
  stopifnot(sum(x)==1)
  length(x) <- 16
  stopifnot(length(x)==16)
  stopifnot(sum(x)==0)
  length(x) <- 32
  stopifnot(length(x)==32)
  stopifnot(sum(x)==0)

  x <- as.bit(ri(1, 1, 32))
  stopifnot(length(x)==32)
  stopifnot(sum(x)==1)
  length(x) <- 16
  stopifnot(length(x)==16)
  stopifnot(sum(x)==1)
  length(x) <- 32
  stopifnot(length(x)==32)
  stopifnot(sum(x)==1)

  x <- as.bitwhich(bit(32))
  stopifnot(length(x)==32)
  stopifnot(sum(x)==0)
  length(x) <- 16
  stopifnot(length(x)==16)
  stopifnot(sum(x)==0)
  length(x) <- 32
  stopifnot(length(x)==32)
  stopifnot(sum(x)==0)

  x <- as.bitwhich(!bit(32))
  stopifnot(length(x)==32)
  stopifnot(sum(x)==32)
  length(x) <- 16
  stopifnot(length(x)==16)
  stopifnot(sum(x)==16)
  length(x) <- 32
  stopifnot(length(x)==32)
  stopifnot(sum(x)==32)

  x <- as.bitwhich(ri(32, 32, 32))
  stopifnot(length(x)==32)
  stopifnot(sum(x)==1)
  length(x) <- 16
  stopifnot(length(x)==16)
  stopifnot(sum(x)==0)
  length(x) <- 32
  stopifnot(length(x)==32)
  stopifnot(sum(x)==0)

  x <- as.bitwhich(ri(2, 32, 32))
  stopifnot(length(x)==32)
  stopifnot(sum(x)==31)
  length(x) <- 16
  stopifnot(length(x)==16)
  stopifnot(sum(x)==15)
  length(x) <- 32
  stopifnot(length(x)==32)
  stopifnot(sum(x)==31)

  x <- as.bitwhich(ri(1, 1, 32))
  stopifnot(length(x)==32)
  stopifnot(sum(x)==1)
  length(x) <- 16
  stopifnot(length(x)==16)
  stopifnot(sum(x)==1)
  length(x) <- 32
  stopifnot(length(x)==32)
  stopifnot(sum(x)==1)

  x <- as.bitwhich(ri(1, 31, 32))
  stopifnot(length(x)==32)
  stopifnot(sum(x)==31)
  message("NOTE the change from 'some excluded' to 'all excluded' here")
  length(x) <- 16
  stopifnot(length(x)==16)
  stopifnot(sum(x)==16)
  length(x) <- 32
  stopifnot(length(x)==32)
  stopifnot(sum(x)==32)



cleanEx()
nameEx("physical")
### * physical

flush(stderr()); flush(stdout())

### Name: physical
### Title: Physical and virtual attributes
### Aliases: physical physical<- virtual virtual<- physical.default
###   physical<-.default virtual.default virtual<-.default print.physical
###   print.virtual
### Keywords: IO data attribute

### ** Examples

  physical(bit(12))
  virtual(bit(12))



cleanEx()
nameEx("regtest.bit")
### * regtest.bit

flush(stderr()); flush(stdout())

### Name: regtest.bit
### Title: Regressiontests for bit
### Aliases: regtest.bit
### Keywords: classes logic

### ** Examples

  if (regtest.bit()){
    message("regtest.bit is OK")
  }else{
    message("regtest.bit failed")
  }

  ## Not run: 
##D     regtest.bit(10000)
##D   
## End(Not run)



cleanEx()
nameEx("repeat.time")
### * repeat.time

flush(stderr()); flush(stdout())

### Name: repeat.time
### Title: Adaptive timer
### Aliases: repeat.time
### Keywords: utilities

### ** Examples

  system.time(1+1)
  repeat.time(1+1)
  system.time(sort(runif(1e6)))
  repeat.time(sort(runif(1e6)))



cleanEx()
nameEx("repfromto")
### * repfromto

flush(stderr()); flush(stdout())

### Name: repfromto
### Title: Virtual recycling
### Aliases: repfromto repfromto<-
### Keywords: IO data

### ** Examples

  message("a simple example")
  repfromto(0:9, 11, 20)



cleanEx()
nameEx("ri")
### * ri

flush(stderr()); flush(stdout())

### Name: ri
### Title: Range index
### Aliases: ri print.ri
### Keywords: classes logic

### ** Examples

 bit(12)[ri(1,6)]



cleanEx()
nameEx("rlepack")
### * rlepack

flush(stderr()); flush(stdout())

### Name: rlepack
### Title: Hybrid Index, rle-pack utilities
### Aliases: rlepack rleunpack rev.rlepack unique.rlepack
### Keywords: IO data

### ** Examples

  x <- rlepack(rep(0L, 10))



cleanEx()
nameEx("setattributes")
### * setattributes

flush(stderr()); flush(stdout())

### Name: setattributes
### Title: Attribute setting by reference
### Aliases: setattributes setattr
### Keywords: attributes

### ** Examples

  x <- as.single(runif(10))
  attr(x, "Csingle")

  f <- function(x)attr(x, "Csingle") <- NULL
  g <- function(x)setattr(x, "Csingle", NULL)

  f(x)
  x
  g(x)
  x

 ## Not run: 
##D 
##D   # restart R
##D   library(bit)
##D 
##D   mysingle <- function(length = 0){
##D     ret <- double(length)
##D     setattr(ret, "Csingle", TRUE)
##D     ret
##D   }
##D 
##D   # show that mysinge gives exactly the same result as single
##D   identical(single(10), mysingle(10))
##D 
##D   # look at the speedup and memory-savings of mysingle compared to single
##D   system.time(mysingle(1e7))
##D   memory.size(max=TRUE)
##D   system.time(single(1e7))
##D   memory.size(max=TRUE)
##D 
##D   # look at the memory limits
##D   # on my win32 machine the first line fails beause of not enough RAM, the second works
##D   x <- single(1e8)
##D   x <- mysingle(1e8)
##D 
##D   # .g. performance with factors
##D   x <- rep(factor(letters), length.out=1e7)
##D   x[1:10]
##D   # look how fast one can do this
##D   system.time(setattr(x, "levels", rev(letters)))
##D   x[1:10]
##D   # look at the performance loss in time caused by the non-needed copying
##D   system.time(levels(x) <- letters)
##D   x[1:10]
##D 
##D 
##D   # restart R
##D   library(bit)
##D 
##D   simplefactor <- function(n){
##D     factor(rep(1:2, length=n))
##D   }
##D 
##D   mysimplefactor <- function(n){
##D     ret <- rep(1:2, length=n)
##D     setattr(ret, "levels", as.character(1:2))
##D     setattr(ret, "class", "factor")
##D     ret
##D   }
##D 
##D   identical(simplefactor(10), mysimplefactor(10))
##D 
##D   system.time(x <- mysimplefactor(1e7))
##D   memory.size(max=TRUE)
##D   system.time(setattr(x, "levels", c("a","b")))
##D   memory.size(max=TRUE)
##D   x[1:4]
##D   memory.size(max=TRUE)
##D   rm(x)
##D   gc()
##D 
##D   system.time(x <- simplefactor(1e7))
##D   memory.size(max=TRUE)
##D   system.time(levels(x) <- c("x","y"))
##D   memory.size(max=TRUE)
##D   x[1:4]
##D   memory.size(max=TRUE)
##D   rm(x)
##D   gc()
##D 
## End(Not run)




cleanEx()
nameEx("unattr")
### * unattr

flush(stderr()); flush(stdout())

### Name: unattr
### Title: Attribute removal
### Aliases: unattr
### Keywords: attribute

### ** Examples

  bit(2)[]
  unattr(bit(2)[])



cleanEx()
nameEx("vecseq")
### * vecseq

flush(stderr()); flush(stdout())

### Name: vecseq
### Title: Vectorized Sequences
### Aliases: vecseq
### Keywords: manip

### ** Examples

  sequence(c(3,4))
  vecseq(c(3,4))
  vecseq(c(1,11), c(5, 15))
  vecseq(c(1,11), c(5, 15), concat=FALSE, eval=FALSE)
  vecseq(c(1,11), c(5, 15), concat=FALSE, eval=TRUE)
  vecseq(c(1,11), c(5, 15), concat=TRUE, eval=FALSE)
  vecseq(c(1,11), c(5, 15), concat=TRUE, eval=TRUE)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
