#' Extract numeric days of month from a \code{Date},
#' \code{POSIXct} or \code{POSIXlt} vector.
#' 
#' Extract numeric days of month from a \link{\code{Date}},
#' \link{\code{POSIXct}} or \link{\code{POSIXlt}} vector. 
#' 
#' @usage Days(x)
#' @param x Date, \link{\code{POSIXct}} or \link{\code{POSIXlt}}
#' vector
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


#' @export
Days.Date <- function(x) {
    as.numeric(format(x, "%d"))
}

#' @export 
Days.POSIXct <- function(x) {
    as.numeric(format(x, "%d"))
}

#' @export 
Days.POSIXlt <- function(x) {
    as.numeric(format(x, "%d"))
}
