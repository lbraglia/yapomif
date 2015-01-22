#' Calculate C index of of follow up completenes
#' 
#' 
#' Calculate C index of of follow up completenes.
#' 
#' 
#' @param time Follow up (in days?)
#' @param status event indicator
#' @param cutoff (in days?)
#' @param strata group
#' @return A list with individual global C-index, strata C-indexes
#' (optional), and individual C-indexes
#' @references Clark T., Altman D., De Stavola B. (2002),
#' Quantification of the completeness of follow-up. Lancet 2002;
#' 359: 1309-10
#' @examples
#'
#' time   <- c(180, 12, 240, 250 )
#' status <- c(  0,  1,   0, 1   )
#' group  <- c("A","A", "B", "B" )
#' 
#' ## example: 
#' ## quantify fup completeness to 200 days (eg minimum potential
#' ## follow up in a hypotethic prospective trial)
#' 
#' fupCompleteness(time = time, status = status, cutoff = 200, strata = group)
#' 
#' @export
fupCompleteness <- function(time = NULL,
                            status = NULL,
                            cutoff = NULL,
                            strata = NULL) {

    ## input validation
    if (sum(sapply(list(time, status, cutoff), is.null))>0) {
        stop("time, status and cutoff needed")
    }

    ## censoring to cutoff ...
    db <- censorAt(time = time,
                    status = status,
                    censor.time = cutoff)
    names(db) <- c("time","status")
    db$potentialFup <- ifelse(db$status == 0, cutoff, db$time)

    ## global and individual C
    res <- list("globalC" = sum(db$time) / sum(db$potentialFup),
                "individualC" = db$time / db$potentialFup )

    ## strata C
    if (!is.null(strata)){
      agg <- aggregate(db[c("time","potentialFup")],
                       by = list(strata),
                       FUN = sum )
      agg$strataC <- agg[,2]/agg[,3]
      res$strataC <-  agg
    }

    return(res)
}




