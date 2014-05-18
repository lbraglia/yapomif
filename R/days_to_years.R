#' Trivial function that converts number of days in number of years.
#' 
#' 
#' Trivial function that converts number of days in number of years.
#' 
#' 
#' @aliases days_to_years days_to_years.numeric
#' @usage days_to_years(days)
#' @param days Numeric vector of days
#' @return A numeric vector of years.
#' @keywords day days year years
#' @examples
#' 
#' days <- seq(1, 365.25*3, by = 60)
#' days_to_years(days)
#' 
#' @export days_to_years
days_to_years <- function(days) {

    UseMethod("days_to_years")
}

#' @export days_to_years.numeric
days_to_years.numeric <- function(days) {
    days / 365.25
    ## mean(c(365,365,365,366))
    
}

