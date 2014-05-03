dadb <- function(tn,tp,fn,fp) {

    test <- factor(rep(c("+","-"), c(tp+fp,tn+fn)), levels=c("-","+"))
    refstd <- factor(c(rep("present", tp),
                       rep("absent", fp),
                       rep("present", fn),
                       rep("absent", tn)),
                     levels=c("absent","present"))

    data.frame(test, refstd)
}
