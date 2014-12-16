#' check if a object is unique
#'
#' check if a object is unique; this is a simple wrapper useful for code
#' readability
#' @param x a object that can be passed to \code{\link{duplicated}}
#' @rdname is_unique
#' @export
is.unique <- function(x = NULL) {
	!(any(duplicated(x)))
}
