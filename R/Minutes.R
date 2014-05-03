Minutes <- function(x) {

  if ( !(class(x)[1] %in% c("POSIXct","POSIXlt")) )
    stop("Only POSIXct and POSIXlt class dates")

  UseMethod("Minutes")
 
}


Minutes.POSIXct <- function(x) {
    as.numeric(format(x, "%M"))
}

Minutes.POSIXlt <- function(x) {
    as.numeric(format(x, "%M"))
}
