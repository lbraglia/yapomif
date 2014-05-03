getExcelSheet <- function(file=NULL, sheet=NULL, ...) {
    ## Funzione per semplificare l'accesso a dati di un sheet di un file di excel
    ## require(RODBC)
    
    on.exit( if (exists("con")) {
        odbcClose(con)
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
        connect.func <- odbcConnectExcel
    } else if (tolower(file.format)=="xlsx") {
        connect.func <- odbcConnectExcel2007
    } else {
        stop(sprintf("Not an '.xls' or '.xlsx': aborted.", file.format))
    }
    
    con <- connect.func(file)
    sqlFetch(con, sheet, ...)
    
    ## Example: getExcelSheet(file="C:/ps1.tab", sheet="es_ps")
    
}
