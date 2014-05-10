#' @name open_xlsx
#' @title Open an xlsx file
#' @author Luca Braglia
#' @description This function tries to open an xlsx file with the
#' proper application, in a portable manner. The user can set up
#' the application, giving its path (or setting the option
#' \code{open_xlsx.bin}); otherwise the handler path is seeked.
#' @param xlsx path to the \code{xlsx} file
#' @param bin path to the application bin
#' @usage open_xlsx(xlsx=NULL, bin=getOption("open_xlsx.bin", find_xlsx_bin()))
#' @export open_xlsx
open_xlsx <- function(xlsx = NULL,
                      bin = getOption("open_xlsx.bin",
                          find_xlsx_bin())) 
{

    ## ---------------------------------------------
    ## begin finder function
    find_xlsx_bin <- function() {
        ## if you find it then return the path. This is system
        ## specific 
        if (Sys.info()["sysname"]=="Linux") {
            con <- pipe("which libreoffice")
            res <- readLines(con)
            close(con)
            if ("" != res) {
                return(res)
            }
        } else if (Sys.info()["sysname"]=="Windows"){
            ## is it needed given shell.exec?
            warning("To be implemented yet")
        }

        ## otherwise return "none"
        return("none")
    }
    ## ---------------------------------------------
    ## begin open.xlsx function
    if ("none" == bin) 
        stop("no suitable program available")                              
    if (is.null(xlsx))
        stop("a file have to be specified")

    ## execution should be in background in order not to block R
    ## interpreter; in linux this is achieved postponing & at the
    ## end of the command
    
    if (Sys.info()["sysname"]=="Linux") {
        my.command <- paste(bin, xlsx, "&")
        system(my.command)
    } else if (Sys.info()["sysname"]=="Windows"){
        shell.exec(xlsx)
    }
    
}
