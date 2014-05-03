xtile <- function(x, n) {
  cuts <- quantile(x, probs=seq(0,1,length=n+1))
  cut(x, breaks=cuts, include.lowest=TRUE)
}