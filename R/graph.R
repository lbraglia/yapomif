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
