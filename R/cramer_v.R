#' 
#' Cramer's V calculator.
#' 
#' 
#' Cramer's V calculator.
#' 
#' 
#' @usage cramer_v(x)
#' @param x an R object. Currently implemented only for tables.
#' @return A scalar with Cramer's V
#' @keywords Cramer V Phi
#' @examples
#' 
#' a <- rnorm(1000) > 0.2
#' b <- gl(2,500)
#' cramer_v(table(a,b))
#' 
#' 
#' @export cramer_v
cramer_v <- function(x) {
    UseMethod("cramer_v")
}

#' @export 
cramer_v.table <- function(x) {
    N <- sum(x)
    k <- min(dim(x))
    if (k < 2)
        stop("k = 1")
    
    unname(sqrt(chisq.test(x)$statistic / (N*(k-1 ))))
}
