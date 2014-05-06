#' Translates from cm (centimeters) to inches.
#' 
#' 
#' Translates from cm (centimeters) to inches.
#' 
#' 
#' @usage inches(cm)
#' @param cm Numeric vector of centimeter.
#' @return A vector with inches.
#' 
#' %% ~Describe the value returned %% If it is a LIST, use %% \item{comp1
#' }{Description of 'comp1'} %% \item{comp2 }{Description of 'comp2'}
#' @keywords inches cm
#' @export inches
inches <- function(cm) {
    cm * 0.393700787
}
