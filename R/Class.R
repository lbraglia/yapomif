#' Extract class smartly
#'
#' Extract class smartly from more complex data object rather than atomic
#' vector (eg data.frame, list) in a handy way
#' 
#' @param x an R object (data.frame or list)
#' @return A vector of classes
#' @keywords class typeof
#' @examples
#' 
#' data(airquality)
#' Class(airquality)
#' 
#' 
#' @export Class
Class <- function(x) {
    UseMethod("Class")
}

#' @export 
Class.data.frame <- function(x) {
    unlist(sapply(x,class))
}

#' @export 
Class.list <- function(x) {
    lapply(x,class)
}
