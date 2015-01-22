#' Trivial function that converts number of days in number of years.
#' 
#' 
#' Trivial function that converts number of days in number of years.
#' 
#' 
#' @param days Numeric vector of days
#' @return A numeric vector of years.
#' @examples
#' 
#' days <- seq(1, 365.25*3, by = 60)
#' daysToYears(days)
#' 
#' @export daysToYears
daysToYears <- function(days) {

    UseMethod("daysToYears")
}

#' @export 
daysToYears.numeric <- function(days) {
    days / 365.25
    ## mean(c(365,365,365,366))
    
}

