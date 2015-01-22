#' Extract numeric hours of month from a POSIXct or POSIXlt vector.
#' 
#' 
#' Extract numeric hours of month from a POSIXct or POSIXlt vector.
#' 
#' 
#' @param x POSIXct or POSIXlt vector
#' @return A numeric vector.
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

#' @export
Hours.POSIXct <- function(x) {
    as.numeric(format(x, "%H"))
}

#' @export
Hours.POSIXlt <- function(x) {
    as.numeric(format(x, "%H"))
}
