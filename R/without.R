#' Remove elements from a vector, chosing from another vector
#' 
#' Return a vector x without the commont elements to another vector y.
#'
#' @param x a vector
#' @param y a vector
#' @export
#' @rdname without
"%without%" <-  function(x, y) x[!(x %in% y)]


