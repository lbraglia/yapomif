#' Calculate diagnostic accuracy measures for several cutoffs of a quantitative
#' marker.
#' 
#' Calculate diagnostic accuracy measures for several cutoffs of a quantitative
#' marker.
#' 
#' @param cutoffs Cutoffs considered
#' @param test Test
#' @param refstd Reference standard
#' @param round.dig Rounding digits
#' @param ... parameters passed to da
#' @return A data.frame for diagnostic accuracy studies.
#' @keywords diagnostic accuracy cutoff
#' @export da_at_cutoff
da_at_cutoff <- function(cutoffs=NULL, 
                         test=NULL,
                         refstd=NULL,
                         round.dig=4,
                         ## parameters passed to da
                         ...
                          ){
    res <- list()
    
    for (tres in cutoffs)  {
        index <- which(cutoffs %in% tres)
        tmp <- 	da(test=(test > tres),  
                    refstd=refstd,  
                    round.dig=round.dig,
                    ...)[["stats"]]
        tmp$thresh <- tres
        res[[index]] <- tmp[c(5,1:4)]
    }
    
    do.call("rbind", res)
	
    ## Esempio chesi procalcitonina
    ## pct.gram <- roc(gram ~ pct, 
    ## direction="<", 
    ## data = db
    ## )
    ## ## Per verifiche a manina
    ## listRoc <- unclass(pct.gram)
    ## thresh.db <- da.at.cutoff(cutoffs=listRoc$thresholds,
    ## test=db$pct,
    ## refstd=db$gram)
    ## th.spl <- split(thresh.db, thresh.db$stat )
    ## ppv <- th.spl$PPV
    ## npv <- th.spl$NPV

	
}



## da.at.cutoff.old <- function(cutoffs=NULL, 
##                           test=NULL,
##                           refstd=NULL,
##                           round.dig=4
##                           ) 
## {
##     res <- list()
    
##     for (tres in cutoffs)  {
##         index <- which(cutoffs %in% tres)
##         tmp <- 	da2(test=(test > tres),  
##                     refstd=refstd,  
##                     round.dig=round.dig)[["stats"]]
##         tmp$thresh <- tres
##         res[[index]] <- tmp[c(5,1:4)]
##     }
    
##     do.call("rbind", res)
    
##     ## Esempio chesi procalcitonina
##     ## pct.gram <- roc(gram ~ pct, 
##     ## direction="<", 
##     ## data = db
##     ## )
##     ## # Per verifiche a manina
##     ## listRoc <- unclass(pct.gram)
##     ## thresh.db <- da.at.cutoff(cutoffs=listRoc$thresholds,
##     ## 					test=db$pct,
##     ## 					refstd=db$gram)
##     ## th.spl <- split(thresh.db, thresh.db$stat )
##     ## ppv <- th.spl$PPV
##     ## npv <- th.spl$NPV

	
## }
