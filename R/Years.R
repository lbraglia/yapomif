#' Extract numeric years from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' Extract numeric years from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' @aliases Years Years.Date Years.POSIXct Years.POSIXlt
#' @usage Years(x)
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

#' @S3method Years Date
Years.Date <- function(x) {
    as.numeric(format(x, "%Y"))
}

#' @S3method Years POSIXct
Years.POSIXct <- function(x) {
    as.numeric(format(x, "%Y"))
}

#' @S3method Years POSIXlt
Years.POSIXlt <- function(x) {
    as.numeric(format(x, "%Y"))
}
