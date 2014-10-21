#' Determine all duplicate elements
#' 
#' 
#' base::duplicated determines which elements of a vector or data frame are
#' duplicates of elements with smaller subscripts, and returns a logical vector
#' indicating which elements (rows) are duplicates.
#' 
#' This wrapper allows to find all duplicated obs.
#' 
#' 
#' @usage
#' 
#' Duplicated( x, all.dup = TRUE, ...)
#' @param x Numeric vector of centimeter.
#' @param all.dup Logical: if TRUE considers duplicated each observation
#' present more than one time in the object, otherwise implement
#' base::duplicated algorithm (considers duplicated the second, third and so on
#' duplicated observation).
#' @param ... Other arguments passed to base::duplicated.
#' @return A logical vector with duplicated marked as TRUE.
#' @keywords duplicated
#' @export Duplicated
Duplicated <- function( x, all.dup = TRUE, ...) {
    if (all.dup) {
        base::duplicated(x, ...)| base::duplicated(x, fromLast = TRUE)
    } else
        base::duplicated(x, ...)
}
