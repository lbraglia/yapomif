#' Open a vectorial device file in a portable way.
#' 
#' 
#' Portable function which choose what (vectorial) device to open looking at
#' the system.
#' 
#' 
#' @usage graph(file, width, height, ...)
#' @param file path without extension
#' @param width width in inches
#' @param height height in inches
#' @param ... other arguments passed to \code{\link{pdf}} or
#' \code{\link[grDevices]{win.metafile}}
#' @return Values returned by pdf or win.metafile?
#' @keywords pdf win.metafile vectorial graph
#' @export graph
graph <- function(file="", width = 7,
                  height = 7,  ...) {
    
    ## funzione portabile tra linux e windows che 
    ## in base al sistema plotta nel device 
    ## opportuno (pdf o win.metafile), posto che gli si 
    ## dia il path al file opportuno (estensione esclusa
	
    if (Sys.info()["sysname"]=="Linux") {
        path <- paste(file, "pdf" ,sep=".")
        pdf(file=path, width = width, height = height, ...)
    } else if (Sys.info()["sysname"]=="Windows") {
        path <- paste(file, "emf" ,sep=".")
        win.metafile(filename=path, width = width, height = height, ...)
    }
}
