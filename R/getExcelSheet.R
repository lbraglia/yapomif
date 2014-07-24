#' Retrieve an MS Excel (R) sheet from an \code{xls} or \code{xlsx} file.
#' 
#' This function is a wrapper around \code{RODBC} function to get a
#' sheet from an \code{xls} or \code{xlsx} file.
#' 
#' @usage getExcelSheet(file=NULL, sheet=NULL, ...)
#' @param file path to the \code{xls} \code{xlsx} file
#' @param sheet sheet name
#' @param ... other arguments passed to \code{\link{sqlFetch}}
#' @return The function return a data.frame with data from the specified sheet.
#' @keywords Excel sheet get
#' @examples
#' 
#'    \dontrun{
#' getExcelSheet(file="C:/foo.xls", sheet="es_ps")
#' }
#' 
#' @export getExcelSheet
getExcelSheet <- function(file=NULL, sheet=NULL, ...) {
    on.exit( if (exists("con")) {
        RODBC::odbcClose(con)
        rm(con) 
    })

    ## Check call
    if (any(is.null(c(file, sheet)))) 
        stop("Must provide both a file (xls/xlsx) path and a sheet name")
		
    if ( ! file.exists( file ) ) stop(sprintf("File '%s' not found", file))
		
    ## Connect with proper driver
    file.format <- strsplit(file, "\\.")[[1]]
    file.format <- file.format[length(file.format)]
    
    if (tolower(file.format)=="xls") {
        connect.func <- RODBC::odbcConnectExcel
    } else if (tolower(file.format)=="xlsx") {
        connect.func <- RODBC::odbcConnectExcel2007
    } else {
        stop(sprintf("Not an '.xls' or '.xlsx': aborted.", file.format))
    }
    
    con <- connect.func(file)
    RODBC::sqlFetch(con, sheet, ...)
}
