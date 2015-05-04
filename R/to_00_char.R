#' Transform an integer vector to a character prefixed by an arbitrary
#' number of 0 digits
#' 
#' Transform an integer vector to a character prefixed by an arbitrary
#' number of 0 digits
#'
#' @param x a numeric vector
#' @param clen length of final character vector
#' @examples
#' to_00_char(c(1,20,3), 3)
#' to_00_char(c(1,20,3), 2)
#' \dontrun{
#' to_00_char(c(1,20,3), 1)
#' }
#' @export
to_00_char <- function(x, clen = NULL){

  ## if(!is.integer(x))
  ##   stop("x must be integer")
  x <- as.character(x)
  xlen <- nchar(x)
  zerolen <- clen - xlen
  if(any(zerolen < 0))
    stop("Some values have length > of clen")    
  zeros <- unlist(lapply(zerolen, function(x) {
    paste(rep(0, x), collapse = "")
  }))
  rval <- paste(zeros, x, sep = "")
  rval
}
