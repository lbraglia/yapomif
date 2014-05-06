#' Vector recode utility
#' 
#' 
#' Yet another recode utility
#' 
#' 
#' @usage recode(from=NULL, rec.direct=NULL)
#' @param from Vector to be recoded.
#' @param rec.direct matrix containing 2 columns; first is "from", second is
#' "to".
#' @return A vector with new codes (if/where modified).
#' 
#' %% ~Describe the value returned %% If it is a LIST, use %% \item{comp1
#' }{Description of 'comp1'} %% \item{comp2 }{Description of 'comp2'}
#' @keywords recode replace
#' @export recode
recode <- function(from=NULL, rec.direct=NULL) {
	
    
    ## A few data input checks
    if( !is.vector(from) ) 
        stop("A vector to be recoded must be given.")
    if( !is.matrix(rec.direct)| ncol(rec.direct)!=2 ) 
        stop("I need a 2 column matrix with recoding directives: ",
             "first column is 'from', second 'to'.")
	
    ## Checking for recoding directives uniqueness
    rec.direct <- unique(rec.direct)
    if( any(duplicated(rec.direct[,1])) )
        stop("No univocal recoding directives")

    to.be.recoded <- from %in% rec.direct[,1]
    
    search.n.replace <- function(search.me, where, replace.me) {
        my.res <- rep(NA, length(where))
        my.res[ where  %in%  search.me] <- replace.me
        my.res
    }
	
    partial.results <- unname(apply(rec.direct, 1,
                                    function(x) search.n.replace(
                                        search.me=x[1],
                                        where=from,
                                        replace.me=x[2]) )) 
	
    recoded.and.NA <- unlist(apply( partial.results, 1,
                                   function(x) ifelse(
                                       any(!is.na(x)) ,
                                       x[!is.na(x)], NA ) ) ) 
	
    ## A value could be NA due to a recode; because of this
    ## ...(is.na(recoded.and.NA) & to.be.recoded) 
    ifelse( !is.na(recoded.and.NA) | (is.na(recoded.and.NA) & to.be.recoded),
           recoded.and.NA, 
           from)

    ## Example
    ## test <- c(1:10,NA)
    ## recode.m <- matrix(c(1,2,NA,3,4,1), nrow=3,ncol=2)
    ## cbind(test, recode( test, recode.m))
    
    ## test2 <- c(letters[1:10],NA)
    ## recode.m2 <- matrix(c(c(letters[1:3]),c(NA,LETTERS[2:3])),
    ## nrow=3,ncol=2) 
    ## cbind(test2, recode(test2, recode.m2))

    ## recode.m3 <- matrix(c(1,2,4,4,5:8),ncol=2)
    ## test3 <- c(1:10,NA)
    ## cbind(test3, recode(test3, recode.m3))

}
