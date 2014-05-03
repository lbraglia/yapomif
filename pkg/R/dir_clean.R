dir.clean <- function(directory) {
    files <- dir(directory)
    ## Se ci sono file, cancellali
    length(files) == 0 ||
        file.remove(file.path(directory, files))
    
}

