#' Pretty print for p-values
#' 
#' 
#' Pretty print for p-values.
#' 
#' 
#' @usage pretty_pval(pvalue, space)
#' @param pvalue A numeric vector of p-values.
#' @param space Logical specifying whether a space should be inserted between
#' number and operator (= or <, default to FALSE).
#' @return The function return a string with the pretty printed p-values.
#' @keywords pretty print p-value p-values p value values
#' @examples
#'
#' pretty_pval(0.3)
#' 
#' pval1 <- c(3, NA, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05)
#' pretty_pval(pval1, space = TRUE)
#'
#' 
#' 
#' @export pretty_pval
pretty_pval <- function(pvalue, space = FALSE) {

    old.scipen <- options("scipen")
    on.exit( options("scipen" = old.scipen) )
    options("scipen"= 999) 
    
    ## Pretty printing for p-value 
    if (any(wrong <- (pvalue> 1 | pvalue <0), na.rm = TRUE)) {
        warning("Not all p-values in [0,1], ")
        pvalue[wrong] <- NA
    }
    
    worker <- function(x, space) {
        if (is.na(x)) {
            return(NA_character_)
        } else if (x < 0.0001) {
            if (space) {
                return("< 0.0001")
            } else {
                return("<0.0001")
            }
        } else if (x < 1) {
            char <- as.character(round(x,4))
            return(paste0(ifelse(space, " ",""),char)) 
        } else {
            return(paste0(ifelse(space, " ",""),"1"))
        }

    }

    unlist(lapply(pvalue, worker, space = space))
    
}
