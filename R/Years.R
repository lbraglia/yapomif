#' Extract numeric years from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' Extract numeric years from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' @param x Date, POSIXct or POSIXlt vector
#' @return A numeric vector.
#' @keywords year Year years Years date Date
#' @examples
#' 
#' Sys.Date()
#' Years(Sys.Date())
#' Years(as.POSIXct(Sys.Date()))
#' Years(as.POSIXlt(Sys.Date()))
#' 
#' 
#' @export Years
Years <- function(x) {

  if ( !(class(x)[1] %in% c("Date","POSIXct","POSIXlt")) )
    stop("Only Date, POSIXct and POSIXlt class dates")

  UseMethod("Years")
 
}

#' @export 
Years.Date <- function(x) {
    as.numeric(format(x, "%Y"))
}

#' @export 
Years.POSIXct <- function(x) {
    as.numeric(format(x, "%Y"))
}

#' @export 
Years.POSIXlt <- function(x) {
    as.numeric(format(x, "%Y"))
}
