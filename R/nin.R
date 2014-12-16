#' Inverse Value Matching
#'
#' 
#' Complement of \code{\%in\%}. Returns the elements of \code{x}
#' that are not in \code{y}.
#'
#' 
#' @param x a vector
#' @param y a vector
#' @export
#' @rdname nin
"%nin%" <- function(x, y) {
    return( !(x %in% y) )
}

