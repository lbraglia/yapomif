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
  date_mdy(Months(x, string = FALSE) + months,
           Days(x) + days,
           Years(x) + years )
}
