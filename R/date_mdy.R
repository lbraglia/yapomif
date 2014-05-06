#' Create a \code{\link{Date}} from month, day and year.
#' 
#' 
#' Create a \code{\link{Date}} from month, day and year.
#' 
#' 
#' @usage date_mdy( month, day, year)
#' @param month Numeric vector of months
#' @param day Numeric vector of days
#' @param year Numeric vector of years
#' @return A vector of \code{\link{Date}}.
#' @keywords month day year date Date months days years
#' @examples
#' 
#' db <- data.frame(month=10:12, day=15:17, year=1982:1984)
#' date_mdy(month = db$month, day= db$day, year=db$year)
#' 
#' 
#' @export date_mdy
date_mdy <- function( month, day, year) {
	as.Date(sprintf("%s-%s-%s", year, month, day))
	# db <- data.frame(month=10:12, day=15:17, year=1982:1984)
	# date.mdy(month = db$month, day= db$day, year=db$year)
}	
