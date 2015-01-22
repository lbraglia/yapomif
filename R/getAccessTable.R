#' Retrieve an MS Access (R) table from an \code{mdb} or \code{accdb} file.
#' 
#' 
#' This function is a wrapper around \code{RODBC} function to get a
#' table from an \code{mdb} or \code{accdb} file. 
#' 
#' 
#' @param file path to the \code{mdb} or \code{accdb} file
#' @param table table name
#' @param user username (login to the file)
#' @param pw password (login to the file)
#' @param ... other arguments passed to \code{\link{sqlFetch}}
#' @return The function return a data.frame with data from the specified table.
#' @examples   \dontrun{
#'  getAccessTable(file="C:/prova.accdb", table="Tabella1")
#' }
#' 
#' @export
getAccessTable <- function(file=NULL, table=NULL, user="admin", pw=NULL, ...) {
    ## Funzione per semplificare l'accesso a dati access
    ## require(RODBC)
	
    on.exit( if (exists("con")) {
        RODBC::odbcClose(con)
        rm(con) 
    })
    
    ## Check call
    if (any(is.null(c(file, table)))) 
        stop("Must provide both a database path and a table name")
    
    if ( ! file.exists( file ) ) stop(sprintf("File '%s' not found", file))
    
    ## Connect with proper 'driver' (minimal)
    file.format <- strsplit(file, "\\.")[[1]]
    file.format <- file.format[length(file.format)]
    
    if (tolower(file.format)=="mdb") {
        connect.func <- RODBC::odbcConnectAccess
    } else if (tolower(file.format)=="accdb") {
        connect.func <- RODBC::odbcConnectAccess2007
    } else {
        stop(sprintf("Not an '.mdb' or '.accdb': aborted.", file.format))
    }
    
    ## Otherwise connect
    con <- connect.func(file, uid=user, pwd=pw )
    RODBC::sqlFetch(con, table, ...)
       
}
