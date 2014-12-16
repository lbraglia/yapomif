#' Generate a data.frame of diagnostic accuracy study.
#' 
#' Generate a data.frame of diagnostic accuracy study given true negative, true
#' positive, false negative, false positive.
#' 
#' 
#' @param tn True negative
#' @param tp True positive
#' @param fn False negative
#' @param fp False positive
#' @return A data.frame for diagnostic accuracy studies.
#' @keywords diagnostic accuracy
#' @examples
#' 
#' db <- dadb(tn=720 , tp=190 , fn=10 , fp=80)
#' with(db, table(test,refstd))
#' 
#' @export dadb
dadb <- function(tn,tp,fn,fp) {

    test <- factor(rep(c("+","-"), c(tp+fp,tn+fn)), levels=c("-","+"))
    refstd <- factor(c(rep("present", tp),
                       rep("absent", fp),
                       rep("present", fn),
                       rep("absent", tn)),
                     levels=c("absent","present"))

    data.frame(test, refstd)
}
