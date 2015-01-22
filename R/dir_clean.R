#' Remove all files in a directory.
#' 
#' Remove all files in a directory, by deleting the directory and creating
#' a new one with the same name.
#' 
#' @rdname dir_clean
#' @param directory Character. A path to directory to be cleaned
#' @export dir.clean
dir.clean <- function(directory) {
  unlink(directory, recursive = TRUE)
  dir.create(directory)
}

