Years <- function(x) {

  if ( !(class(x)[1] %in% c("Date","POSIXct","POSIXlt")) )
    stop("Only Date, POSIXct and POSIXlt class dates")

  UseMethod("Years")
 
}


Years.Date <- function(x) {
    as.numeric(format(x, "%Y"))
}

Years.POSIXct <- function(x) {
    as.numeric(format(x, "%Y"))
}

Years.POSIXlt <- function(x) {
    as.numeric(format(x, "%Y"))
}
