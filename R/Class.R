#' Extract class smartly from more complex data object rather than atomic
#' vector (eg data.frame) in a handy way
#' 
#' 
#' Extract numeric hours of month from a POSIXct or POSIXlt vector.
#' 
#' 
#' @aliases Class Class.data.frame
#' @usage Class(x)
#' @param x an R object
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

#' @export Class data.frame
Class.data.frame <- function(x) {
    unlist(sapply(x,class))
}
