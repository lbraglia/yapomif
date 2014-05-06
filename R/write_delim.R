#' Write a tab separated file with "." as decimal separator.
#' 
#' 
#' This function is a wrapper around read.table to write tab separated files
#' with "." as decimal separator.
#' 
#' @rdname write_delim
#' @usage
#' 
#' write.delim(x=NULL, file="", row.names=FALSE, col.names=TRUE, quote=TRUE,
#' append=FALSE, na="", dec=".")
#' @param x Matrix or data.frame to be exported
#' @param file file parameter of write.table
#' @param row.names row names
#' @param col.names col names
#' @param quote quote
#' @param append append
#' @param na NA string
#' @param dec Decimal separator
#' @return Nothing returned??
#' 
#' %% ~Describe the value returned %% If it is a LIST, use %% \item{comp1
#' }{Description of 'comp1'} %% \item{comp2 }{Description of 'comp2'}
#' @keywords tab delimited format file output
write.delim <- function(x=NULL,
                        file="",
                        row.names=FALSE,
                        col.names=TRUE,
                        quote=TRUE,
                        append=FALSE,
                        na="",
                        dec="."){
    write.table(x = x,
                file=file,
                sep="\t",
                dec=dec,
                row.names=row.names,
                col.names=col.names,
                quote=quote,					
                append=append,
                na=na
                )
}
