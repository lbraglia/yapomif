#' Suggest some available methods about and object
#'
#' Suggest some available methods about and object
#' @param x an object
#' @export
hint <- function(x = NULL) methods(class = class(x))
