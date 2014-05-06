#' 
#' Path-function generator.
#' 
#' 
#' This function creates another function that paste the directory set up in
#' the call to \code{make_path} to the argument passed in the current call,
#' allowing better directory parametrazion, auto-explaining code and less
#' typing. See example for usage.
#' 
#' 
#' @aliases make_path make_path.coxph make_path.survreg
#' @usage
#' 
#' make_path(path)
#' @param path Base path of interest.
#' @return A function that paste the directory set up in the call to
#' \code{make_path} to the argument passed in the current call.
#' @keywords path
#' @examples
#' 
#' res.dir <- make_path("results")
#' tmp.dir <- make_path("/tmp")
#' 
#' ## Eg for use with write.csv() or pdf()
#' res.dir("my_results.csv")
#' tmp.dir("graph.pdf")
#' 
#' 
#' @export make_path
make_path <- function(path) {
    paste_path <- function(file=NULL) {

        if (is.null(file)) {
            file <- ""
        }
        
        paste(path, file, sep="/")
    }
    paste_path
}
