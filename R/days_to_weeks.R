days_to_weeks <- function(days) {

    UseMethod("days_to_weeks")
}

days_to_weeks.numeric <- function(days) {
    days / 7
    
}

