Months <- function(x, string = TRUE, abbreviate = FALSE) {

  if ( !(class(x)[1] %in% c("Date","POSIXct","POSIXlt")) )
    stop("Only Date, POSIXct and POSIXlt class dates")

  UseMethod("Months")
 
}


Months.Date <- function( x, string = TRUE, abbreviate = FALSE ) {
    if (!string) {
        return(as.numeric(format(x=x, "%m")))
    } else {
        return(format(x=x, ifelse(abbreviate, "%b", "%B")))
    }
    
}

Months.POSIXct <- function( x, string = TRUE, abbreviate = FALSE ) {
    if (!string) {
        return(as.numeric(format(x=x, "%m")))
    } else {
        return(format(x=x, ifelse(abbreviate, "%b", "%B")))
    }
    
}

Months.POSIXlt <- function( x, string = TRUE, abbreviate = FALSE ) {
    if (!string) {
        return(as.numeric(format(x=x, "%m")))
    } else {
        return(format(x=x, ifelse(abbreviate, "%b", "%B")))
    }
    
}
