#' Not in
#' 
#' Negation of the \code{%in%} operator.
#' 
#' @rdname not-in
#' @aliases %nin%
#' @usage x %nin% table
#' @param x The values to be matched
#' @param table The values to \emph{not} be matched against
#' @return Logical vector, negation of the \code{%in%} operators on the same
#' arguments.
#' @author Original author: Romain Francois
#' @keywords not in
#' @examples
#'                         
#' 1:10 %nin% c(1,3,5,9)  
#' 
`%nin%` <- function(x,table) {
    !(`%in%`(x,table))
}

