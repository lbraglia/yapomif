cramer_v <- function(x) {
    UseMethod("cramer_v")
}

cramer_v.table <- function(x) {
    N <- sum(x)
    k <- min(dim(x))
    if (k < 2)
        stop("k = 1")
    
    unname(sqrt(chisq.test(x)$statistic / (N*(k-1 ))))
}
