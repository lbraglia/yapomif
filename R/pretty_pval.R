#' Pretty print for p-values
#' 
#' 
#' Pretty print for p-values.
#' 
#' 
#' @usage pretty_pval(pvalue, space)
#' @param pvalue A numeric vector of p-values.
#' @param space Logical specifying whether a space shoud be inserted between
#' number and operator (= or <, default to FALSE).
#' @return The function return a string with the pretty printed p-values.
#' 
#' %% ~Describe the value returned %% If it is a LIST, use %% \item{comp1
#' }{Description of 'comp1'} %% \item{comp2 }{Description of 'comp2'}
#' @keywords pretty print p-value p-values p value values
#' @export pretty_pval
pretty_pval <- function(pvalue, space = FALSE) {

    old.scipen <- options("scipen")
    on.exit( options("scipen" = old.scipen) )
    options("scipen"= 999) 
    
    ## Pretty printing for p-value 
    if (any(pvalue> 1 | pvalue <0) & !is.na(pvalue))
        stop("p-value must be in [0,1], or NA")

    worker <- function(x, space) {
        if (x < 0.0001) {
            if (space) {
                return("< 0.0001")
            } else {
                return("<0.0001")
            }
        } else if (x < 1) {
            char <- as.character(round(x,4))
            if(char=="1") {
                return(paste("=",char,sep=ifelse(space, " ",""))) 
            } else {
                return(paste("=",
                             char,
                             sep=ifelse(space, " ","")))
            }
        } else {
            return(paste("=","1", sep=ifelse(space, " ",""))  )
        }

    }

    ## Vectorized
    vec.worker <- Vectorize(worker)
    vec.worker(pvalue, space = space)
    
    ## Unvectorized
    ## worker(x = pvalue, space = space)
    
}

## debug(pretty_pval)
## pretty_pval(0.00056)
