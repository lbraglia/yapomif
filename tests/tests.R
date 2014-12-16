library(yapomif)

assert <- function(x) {
  if (! x) stop(paste("Assertion failed:", deparse(substitute(x))))
}

## nin
assert(1 %nin% 2:4)
assert("a" %nin% letters[2:4])

## date_mdy, Months, Days, Years
today <- Sys.Date()
assert( identical(today, dateMDY(Months(today, string=FALSE),
                                 Days(today),
                                 Years(today))))

## More Tests needed ...
