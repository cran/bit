.First.lib <- function(lib, pkg) {
  library.dynam("bit", pkg, lib)
  bit_init()
  cat("package:bit (c) 2008 Jens Oehlschlaegel (GPL-2)\n")
  cat("creator: bit\n")
  cat("methods: as.bit as.bit.which which.bit as.logical as.integer length length<- print [ [<- [[ [[<- ! & | xor != == any all sum\n")
  cat("for more help type ?bit\n")
}

.Last.lib <- function(libpath) {
  bit_done()
  library.dynam.unload("bit", libpath)
}

