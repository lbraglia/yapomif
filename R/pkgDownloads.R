#' Retrieve package download logs from cran-logs.r-studio.com
#'
#' Retrieve package download logs from cran-logs.r-studio.com
#' @param from starting \code{Date} (or something coercible to)
#' @param to ending \code{Date} (or something coercible to)
#' @examples
#' \dontrun{
#' ## looking at yesterday's data for aplore3 and aprean3
#' db <- pkgDownloads(from = Sys.Date()-4)
#' dbSel <- db[db$package %in% c("aplore3", "aprean3"), ]
#' res <- with(dbSel, table(date, package))
#' barplot(res, beside = TRUE)
#' }
#' @export
pkgDownloads <- function(from = Sys.Date()-1, to = Sys.Date()-1){
  from <- as.Date(from)
  to <- as.Date(to)
  days <- seq(from, to, by = 'day')
  year <- as.POSIXlt(days)$year + 1900
  filename <- paste0( days, '.csv.gz')
  urls <- paste0('http://cran-logs.rstudio.com/',
                 year, '/', filename)
  dataDir <- tempdir()
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(dataDir)
  mapply(FUN = "download.file", url = urls, destfile = filename)
  system("gunzip *.gz")
  csvs <- list.files(pattern = "*.csv")
  csv.list <- lapply(csvs, read.csv)
  do.call("rbind", csv.list)
}
