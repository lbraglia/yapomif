Class <- function(x) {

    UseMethod("Class")
}

Class.data.frame <- function(x) {
    unlist(sapply(x,class))
}
