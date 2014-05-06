#' summary with sd
#' 
#' 
#' This is a wrapper around summary which adds sd by default.
#' 
#' 
#' @usage enSummary(var)
#' @param var Vector to be passed to summary.
#' @return The function return same results of summary adding sd.
#' 
#' %% ~Describe the value returned %% If it is a LIST, use %% \item{comp1
#' }{Description of 'comp1'} %% \item{comp2 }{Description of 'comp2'}
#' @keywords summary
#' @export enSummary
enSummary <- function(var) c(summary(var), "Sd"=sd(var, na.rm=TRUE))
