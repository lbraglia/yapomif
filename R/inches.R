#' Translates from cm (centimeters) to inches.
#' 
#' 
#' Translates from cm (centimeters) to inches.
#' 
#' 
#' @usage inches(cm)
#' @param cm Numeric vector of centimeter.
#' @return A vector with inches.
#' @keywords inches cm
#' @export inches
inches <- function(cm) {
    cm * 0.393700787
}
