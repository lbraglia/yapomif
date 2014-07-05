#'
#' Return an hex color
#'
#' @param col a color
#' @param alpha alpha shading
#' @export col2hex 
col2hex <- function(col, alpha = 0.3){
    rgb(t(col2rgb(col)), maxColorValue = 255, alpha = alpha*255)
}
