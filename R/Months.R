#' Extract numeric months of the year from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' Extract numeric months of the year from a Date, POSIXct or POSIXlt vector.
#' 
#' 
#' @param x Date, POSIXct or POSIXlt vector
#' @param string Logical, if TRUE months string is returned, otherwise numeric
#' @param abbreviate logical. Should the months in string format be
#' abbreviated?
#' @return A vector with extracted months.
#' @examples
#' 
#' Sys.Date()
#' Months(Sys.Date())
#' Months(Sys.Date(), string = FALSE)
#' Months(Sys.Date(), string = TRUE, abbreviate = TRUE)
#' ## from base package
#' Months(Sys.Date())
#' Months(as.POSIXct(Sys.Date()))
#' Months(as.POSIXlt(Sys.Date()))
#' 
#' 
#' @export Months
Months <- function(x, string = TRUE, abbreviate = FALSE) {

  if ( !(class(x)[1] %in% c("Date","POSIXct","POSIXlt")) )
    stop("Only Date, POSIXct and POSIXlt class dates")

  UseMethod("Months")
 
}

#' @export 
Months.Date <- function( x, string = TRUE, abbreviate = FALSE ) {
    if (!string) {
        return(as.numeric(format(x=x, "%m")))
    } else {
        return(format(x=x, ifelse(abbreviate, "%b", "%B")))
    }
    
}

#' @export 
Months.POSIXct <- function( x, string = TRUE, abbreviate = FALSE ) {
    if (!string) {
        return(as.numeric(format(x=x, "%m")))
    } else {
        return(format(x=x, ifelse(abbreviate, "%b", "%B")))
    }
    
}

#' @export
Months.POSIXlt <- function( x, string = TRUE, abbreviate = FALSE ) {
    if (!string) {
        return(as.numeric(format(x=x, "%m")))
    } else {
        return(format(x=x, ifelse(abbreviate, "%b", "%B")))
    }
    
}
