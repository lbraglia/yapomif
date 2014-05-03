duplicated <- function( x, all.dup = FALSE, ...) {
    if (all.dup) {
        base::duplicated(x, ...)| base::duplicated(x, fromLast = TRUE)
    } else
        base::duplicated(x, ...)
}
