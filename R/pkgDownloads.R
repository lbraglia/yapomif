#' Retrieve package download logs from cran-logs.r-studio.com
#'
#' Retrieve package download logs from cran-logs.r-studio.com
#' @param from starting \code{Date} (or something coercible to)
#' @param to ending \code{Date} (or something coercible to)
#' @examples
#' \dontrun{
#' db <- pkgDownloads(Sys.Date()-4)
#' barplot(with(db, tapply(package %in% "aplore3", date, sum )), las = 2)
#' }
#' @export
pkgDownloads <- function(from = Sys.Date()-2, to = Sys.Date()-1){
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
