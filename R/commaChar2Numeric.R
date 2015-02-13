#' Convert a char with comma to a numeric
#'
#' Convert a char with comma to a numeric
#'
#' @param x vector of chars with commas
#' @return a vector of numeric
#' @examples
#'
#' commaChar2Numeric(c("2,456", "1,234"))
#' 
#' @export
commaChar2Numeric <- function(x) {
  as.numeric(gsub(",", ".", x))
}
