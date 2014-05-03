make_path <- function(path) {
    paste_path <- function(file=NULL) {

        if (is.null(file)) {
            file <- ""
        }
        
        paste(path, file, sep="/")
    }
    paste_path
}
