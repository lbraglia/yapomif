#' Trivial function that converts number of days in number of months.
#' 
#' 
#' Trivial function that converts number of days in number of months.
#' 
#' 
#' @usage days_to_months(days)
#' @param days Numeric vector of days
#' @return A numeric vector of months.
#' @keywords day days month months
#' @examples
#' 
#' days <- 1:30
#' days_to_months(days)
#' 
#' @export days_to_months
days_to_months <- function(days) {

    UseMethod("days_to_months")
}

#' @export
days_to_months.numeric <- function(days) {
    days / 30.43
    ## 30.43 comes from mean(
    ## c(c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,29,31,30,31,30,31,31,30,31,30,31)))
    
}

