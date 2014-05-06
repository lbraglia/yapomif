#' 
#' Remove all files in a directory.
#' 
#' 
#' Remove all files in a directory.
#' 
#' @rdname dir_clean
#' @usage dir.clean(directory)
#' @param directory Character. A path to directory to be cleaned
#' @return Logical. Operation successful or no files in the directory.
#' @keywords directory clean
#' @examples
#' 
#' tmp.dir <- tempdir()
#' file.create(paste(tmp.dir,"foo", sep = "/"))
#' dir.clean(tmp.dir)
#' dir(tmp.dir)
#' 
#' @export dir.clean
dir.clean <- function(directory) {
    files <- dir(directory)
    ## Se ci sono file, cancellali
    length(files) == 0 ||
        file.remove(file.path(directory, files))
    
}

