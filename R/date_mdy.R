date_mdy <- function( month, day, year) {
	as.Date(sprintf("%s-%s-%s", year, month, day))
	# db <- data.frame(month=10:12, day=15:17, year=1982:1984)
	# date.mdy(month = db$month, day= db$day, year=db$year)
}	
