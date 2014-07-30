#' Print a message inside an header
#'
#' Print a message inside an header
#'
#' @param msg message to be printed
#' @param char character used for header printing
#' @export 
header <- function(msg = "" , char = "-") {

    my.head <- paste(c(rep(char, nchar(msg)), "\n"),
                     collapse = "")

    cat("\n", my.head, sep="")
    cat(msg, "\n")
    cat(my.head, "\n")    
}
