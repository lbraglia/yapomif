#' Remove unprintable chars
#'
#' Remove unprintable chars
#' @param string a character vector
#' @export
rmUnprintableChars <- function(string=NULL) gsub("[\001-\037]","", string)
