days_to_months <- function(days) {

    UseMethod("days_to_months")
}

days_to_months.numeric <- function(days) {
    days / 30.43
    ## 30.43 comes from mean(
    ## c(c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,29,31,30,31,30,31,31,30,31,30,31)))
    
}

