#' Enhanced summary
#' 
#' This is a wrapper around summary which adds sd by default.
#' 
#' @param x Vector to be passed to summary.
#' @param ... Further arguments
#' @keywords summary
#' @export enSummary
enSummary <- function(x, ...) UseMethod("enSummary")

#' @export
enSummary.numeric <- function(x, ...) {
  res <- c(summary(x), sd(x, na.rm=TRUE))
  res <- matrix(res, ncol=1)
  rownames(res) <- c(names(summary(x)), "Sd")
  res
}

#' @export
enSummary.default <- function(x, ...) base::summary(x, ...)
