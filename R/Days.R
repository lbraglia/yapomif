Days <- function(x) {

  if ( !(class(x)[1] %in% c("Date","POSIXct","POSIXlt")) )
    stop("Only Date, POSIXct and POSIXlt class dates")

  UseMethod("Days")
 
}


Days.Date <- function(x) {
    as.numeric(format(x, "%d"))
}

Days.POSIXct <- function(x) {
    as.numeric(format(x, "%d"))
}

Days.POSIXlt <- function(x) {
    as.numeric(format(x, "%d"))
}
