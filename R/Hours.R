#' Extract numeric hours of month from a POSIXct or POSIXlt vector.
#' 
#' 
#' Extract numeric hours of month from a POSIXct or POSIXlt vector.
#' 
#' 
#' @aliases Hours Hours.POSIXct Hours.POSIXlt
#' @usage Hours(x)
#' 
#' \method{HoursPOSIXct}(x)
#' 
#' \method{HoursPOSIXlt}(x)
#' @param x POSIXct or POSIXlt vector
#' @return A numeric vector.
#' @keywords hour Hour hours Hours date Date
#' @examples
#' 
#' now <- Sys.time()
#' Hours(now)
#' Hours(as.POSIXlt(now))
#' 
#' 
#' @export Hours
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
