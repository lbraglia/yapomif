#' Generate a data.frame of dummy variables given a vector
#'
#'
#' Generate a data.frame of dummy variables given a vector
#'
#'
#' @usage
#'
#' dummy(x)
#' @param x a vector
#' @return a data.frame to be cbinded
#' @examples
#'
#' rep(1:3, 4)
#' dummy(rep(1:3, 4))
#'
#' @export dummy
dummy <- function(x) as.data.frame(model.matrix( ~ x - 1) )

