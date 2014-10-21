#' Is fisher test needed
#'
#' Is fisher test needed?
#' @param x the first variable
#' @param y the second variable
#' @param threshold cutoff value under indipendence
#' @param fullReport return a full report or only the \code{TRUE}/\code{FALSE} answer
#' @export
fisherNeeded <- function(x, y, threshold=5, fullReport = FALSE) {

  ## Input check
  if(length(x) != length(y))
    stop("x and y don't have the same length")

  ## Result list
  res <- list()
  res$table <- table(x, y)
  ## Under indipendence...
  (cross <- prop.table(table(x)) %o% prop.table(table(y)))
  res$indipendence <- cross*length(x)
  res$use.fisher <- any(res$indipendence <= threshold)

  if (fullReport) {
    return(res)
  } else
    return(res$use.fisher)

}
