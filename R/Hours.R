Hours <- function(x) {

  if ( !(class(x)[1] %in% c("POSIXct","POSIXlt")) )
    stop("Only POSIXct and POSIXlt class dates")

  UseMethod("Hours")
 
}


Hours.POSIXct <- function(x) {
    as.numeric(format(x, "%H"))
}

Hours.POSIXlt <- function(x) {
    as.numeric(format(x, "%H"))
}
