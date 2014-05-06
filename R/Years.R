#' Extract numeric years from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' Extract numeric years from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' @aliases Years Years.Date Years.POSIXct Years.POSIXlt
#' @usage Years(x)
#' 
#' \method{YearsDate}(x)
#' 
#' \method{YearsPOSIXct}(x)
#' 
#' \method{YearsPOSIXlt}(x)
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


Years.Date <- function(x) {
    as.numeric(format(x, "%Y"))
}

Years.POSIXct <- function(x) {
    as.numeric(format(x, "%Y"))
}

Years.POSIXlt <- function(x) {
    as.numeric(format(x, "%Y"))
}
