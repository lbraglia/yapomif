#' Trivial function that converts number of days in number of weeks.
#' 
#' 
#' Trivial function that converts number of days in number of weeks.
#' 
#' 
#' @param days Numeric vector of days
#' @return A numeric vector of weeks.
#' @keywords day days week weeks
#' @examples
#' 
#' days <- 1:30
#' days_to_weeks(days)
#' 
#' @export days_to_weeks
days_to_weeks <- function(days) {

    UseMethod("days_to_weeks")
}

#' @export 
days_to_weeks.numeric <- function(days) {
    days / 7
    
}

