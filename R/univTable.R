#' Univariate table with absolute, relative, cumulative frequencies.
#' 
#' 
#' Univariate table with absolute, relative, cumulative frequencies.
#' 
#' 
#' @param var vector
#' @param totals print totals?
#' @param useNA print NA?
#' @param NA.string character used for NA's columns title
#' @param round.digits number of rounding digits
#' @param sorting sorting can be "\code{asc} or "\code{desc}"
#' @return A matrix with frequencies.
#' @export
univTable <- function(var=NULL,
                      totals=TRUE,
                      useNA="ifany",
                      NA.string = "NA",
                      round.digits=3,
                      ## sorting can be "asc" or "desc"
                      sorting=NULL			
                      ) {
    
    abs.freq <- table(var, useNA=useNA)
    
    if( is.null(sorting) ) {
        ## Do nothing
    } else if(sorting=="desc") {
        ## Descending ordered frequencies
        abs.freq <- rev(sort(abs.freq))
    } else if( sorting=="asc") {
        ## Ascending ordered frequencies
        abs.freq <- sort(abs.freq)
    } else { 
        ##Otherwise, do nothing
    }
	
    rel.freq <- prop.table(abs.freq)
    cum.freq <- cumsum(rel.freq)
	
    result <- cbind(abs.freq,rel.freq,cum.freq)
    colnames(result) <- c("Abs","Rel","Cum")

    if(totals) {
        ##Aggiungo i totali delle prime due righe (freq. assolute e perc)
        Sum <- c(colSums(result)[1:2], NA)
        result <- rbind(result, Sum)
    }

    ## NA
    rownames(result)[is.na(rownames(result))] <- NA.string 
        
    ## Formatting
    round(result, round.digits)
}
