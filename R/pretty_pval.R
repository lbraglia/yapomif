pretty_pval <- function(pvalue, space = FALSE) {

    old.scipen <- options("scipen")
    on.exit( options("scipen" = old.scipen) )
    options("scipen"= 999) 
    
    ## Pretty printing for p-value 
    if (any(pvalue> 1 | pvalue <0))
        stop("p-value must be in [0,1]")

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
