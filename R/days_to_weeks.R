#' Trivial function that converts number of days in number of weeks.
#' 
#' 
#' Trivial function that converts number of days in number of weeks.
#' 
#' 
#' @aliases days_to_weeks days_to_weeks.numeric
#' @usage days_to_weeks(days)
#' 
#' \method{days_to_weeksnumeric}(days)
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

days_to_weeks.numeric <- function(days) {
    days / 7
    
}

