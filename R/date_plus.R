#' Add a period of time to a Date object
#'
#' Add a period of time to a Date object
#' @param x starting Date
#' @param years years to be added
#' @param months months to be added
#' @param days days to be added
#' @examples
#' (today <- Sys.Date())
#' date_plus(today, years = 1, months = 1, days = 1)
#' @export
date_plus <- function(x, years = 0, months = 0, days = 0){
  x <- as.POSIXlt(x)
  x$year <- x$year + years
  x$mon <- x$mon + months
  x$day <- x$day + days
  as.Date(x)
  
}
