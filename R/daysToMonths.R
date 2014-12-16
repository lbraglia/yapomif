#' Trivial function that converts number of days in number of months.
#' 
#' 
#' Trivial function that converts number of days in number of months.
#' 
#' 
#' @param days Numeric vector of days
#' @return A numeric vector of months.
#' @keywords day days month months
#' @examples
#' 
#' days <- 1:30
#' daysToMonths(days)
#' 
#' @export daysToMonths
daysToMonths <- function(days) {

    UseMethod("daysToMonths")
}

#' @export
daysToMonths.numeric <- function(days) {
    days / 30.43
    ## 30.43 comes from mean(
    ## c(c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,29,31,30,31,30,31,31,30,31,30,31)))
    
}

