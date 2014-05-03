univTable <- function(var=NULL,
                      totals=TRUE,
                      useNA="ifany",
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
	
    ## Formatting
    round(result, round.digits)
}
