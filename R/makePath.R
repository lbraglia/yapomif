#' 
#' Path-function generator.
#' 
#' 
#' This function creates another function that paste the directory set up in
#' the call to \code{makePath} to the argument passed in the current call,
#' allowing better directory parametrazion, auto-explaining code and less
#' typing. See example for usage.
#' 
#' 
#' @usage
#' 
#' makePath(path)
#' @param path Base path of interest.
#' @return A function that paste the directory set up in the call to
#' \code{makePath} to the argument passed in the current call.
#' @keywords path
#' @examples
#' 
#' res.dir <- makePath("results")
#' tmp.dir <- makePath("/tmp")
#' 
#' ## Eg for use with write.csv() or pdf()
#' res.dir("my_results.csv")
#' tmp.dir("graph.pdf")
#' 
#' 
#' @export makePath
makePath <- function(path) {
    paste_path <- function(file=NULL) {

        if (is.null(file)) {
            file <- ""
        }
        
        paste(path, file, sep="/")
    }
    paste_path
}
