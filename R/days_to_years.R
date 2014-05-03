days_to_years <- function(days) {

    UseMethod("days_to_years")
}

days_to_years.numeric <- function(days) {
    days / 365.25
    ## mean(c(365,365,365,366))
    
}

