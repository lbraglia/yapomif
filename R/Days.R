#' Extract numeric days of month from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' Extract numeric days of month from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' @aliases Days Days.Date Days.POSIXct Days.POSIXlt
#' @usage Days(x)
#' @param x Date, POSIXct or POSIXlt vector
#' @return A numeric vector.
#' @keywords day Day days Days date Date
#' @examples
#' 
#' Sys.Date()
#' Days(Sys.Date())
#' Days(as.POSIXct(Sys.Date()))
#' Days(as.POSIXlt(Sys.Date()))
#' 
#' 
#' @export Days
Days <- function(x) {

  if ( !(class(x)[1] %in% c("Date","POSIXct","POSIXlt")) )
    stop("Only Date, POSIXct and POSIXlt class dates")

  UseMethod("Days")
 
}


#' @export Days Date
Days.Date <- function(x) {
    as.numeric(format(x, "%d"))
}

#' @export Days POSIXct
Days.POSIXct <- function(x) {
    as.numeric(format(x, "%d"))
}

#' @export Days POSIXlt
Days.POSIXlt <- function(x) {
    as.numeric(format(x, "%d"))
}
