#' Calculate C index of of follow up completenes
#' 
#' 
#' Calculate C index of of follow up completenes.
#' 
#' 
#' @param time Follow up (in days?)
#' @param status event indicator
#' @param cutoff (in days?)
#' @return A list with individual follow up completeness and the
#' C index
#' @references Clark T., Altman D., De Stavola B. (2002),
#' Quantification of the completeness of follow-up. Lancet 2002;
#' 359: 1309-10
#' @keywords time event follow up completeness
#' @examples
#'
#' time   <- c(180, 12, 240)
#' status <- c(  0,  1,   0)
#' 
#' ## example: 
#' ## quantify fup completeness to 200 days (eg minimum potential
#' ## follow up in a hypotethic prospective trial)
#' 
#' fupCompleteness(time = time, status = status, cutoff = 200)
#' 
#' @export fupCompleteness
fupCompleteness <- function(time=NULL, status=NULL, cutoff=NULL) {

    ## input validation
    if (sum(sapply(list(time, status, cutoff), is.null))>0) {
        stop("time, status and cutoff needed")
    }

    ## censoring to cutoff ...
    db <- censor_at(time = time,
                    status = status,
                    censor.time = cutoff)
    names(db) <- c("time","status")
    db$potentialFup <- ifelse(db$status == 0, cutoff, db$time)
    list("C" = sum(db$time) / sum(db$potentialFup),
         "individualC" = db$time / db$potentialFup)
}




