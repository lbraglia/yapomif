library(yapomif)

assert <- function(x) {
  if (! x) stop(paste("Assertion failed:", deparse(substitute(x))))
}

## nin
assert(1 %nin% 2:4)
assert("a" %nin% letters[2:4])

## censor_at TODO

## da TODO

## date_mdy, Months, Days, Years
today <- Sys.Date()
assert( identical(today, date_mdy(Months(today, string=FALSE),
                                  Days(today),
                                  Years(today))))

## fup completeness
time   <- c(180, 12, 240)
status <- c(  0,  1,   0)
results <- structure(list(C = 0.951456310679612,
                          individualC = c(0.9, 1, 1)),
                     .Names = c("C", "individualC"))

assert(all.equal(fupCompleteness(time = time, status = status, cutoff = 200),
                 results
                 ))

