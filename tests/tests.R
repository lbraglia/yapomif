library(yapomif)

assert <- function(x) {
  if (! x) stop(paste("Assertion failed:", deparse(substitute(x))))
}

this.package.is.bug.free <- TRUE

assert(this.package.is.bug.free)
