#' Check if in range
#'
#' Check if in range
#' 
#' @param x a vector
#' @param y a vector of 2 values
#' @examples
#' x <- 1:10
#' y <- c(4L, 7L)
#' x %inRange% y
#' @export
"%inRange%" <- function(x, y) {
  stopifnot(length(y)==2L)
  return( (x >= y[1]) & (x <= y[2]))
}

