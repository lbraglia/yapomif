#' Pretty print for p-values
#' 
#' 
#' Pretty print for p-values.
#' 
#' 
#' @param pvalue A numeric vector of p-values.
#' @param space Logical specifying whether a space should be inserted between
#' number and operator (= or <, default to FALSE).
#' @param equal add = where needed?
#' @return The function return a string with the pretty printed p-values.
#' @keywords pretty print p-value p-values p value values
#' @examples
#'
#' prettyPval(0.3)
#' 
#' pval1 <- c(3, NA, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05)
#' prettyPval(pval1, space = TRUE)
#'
#' 
#' 
#' @export
prettyPval <- function(pvalue, space = FALSE, equal = TRUE) {

    old.scipen <- options("scipen")
    on.exit( options("scipen" = old.scipen) )
    options("scipen"= 999) 
    
    ## Pretty printing for p-value 
    if (any(wrong <- (pvalue> 1 | pvalue <0), na.rm = TRUE)) {
        warning("Not all p-values in [0,1], ")
        pvalue[wrong] <- NA
    }

    ## implement this in C?
    worker <- function(x, space, equal) {
        if (is.na(x)) {
            return(NA_character_)
        } else if (x < 0.0001) {
          return(paste0("<", ifelse(space, " ",""), "0.0001"))
          ## if (space) {
          ##       return("< 0.0001")
          ##   } else {
          ##       return("<0.0001")
          ##   }
        } else if (x < 1) {
            char <- as.character(round(x,4))
            return(paste0(ifelse(equal, "=",""), ifelse(space, " ",""), char)) 
        } else {
            return(paste0(ifelse(equal, "=",""), ifelse(space, " ",""),"1"))
        }

    }

    unlist(lapply(pvalue, worker, space = space, equal = equal))
    
}