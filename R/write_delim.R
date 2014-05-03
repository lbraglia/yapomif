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
