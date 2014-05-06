#' Extract numeric minutes of month from a POSIXct or POSIXlt vector.
#' 
#' 
#' Extract numeric minutes of month from a POSIXct or POSIXlt vector.
#' 
#' 
#' @aliases Minutes Minutes.POSIXct Minutes.POSIXlt
#' @usage Minutes(x)
#' 
#' \method{MinutesPOSIXct}(x)
#' 
#' \method{MinutesPOSIXlt}(x)
#' @param x POSIXct or POSIXlt vector
#' @return A numeric vector.
#' @keywords hour Hour minutes Minutes date Date
#' @examples
#' 
#' now <- Sys.time()
#' Minutes(now)
#' Minutes(as.POSIXlt(now))
#' 
#' 
#' @export Minutes
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
