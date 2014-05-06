#' Cut numeric x in quantile
#' 
#' 
#' Cut numeric x in quantile
#' 
#' 
#' @usage xtile(x,n)
#' @param x numeric vector
#' @param n number of quantiles
#' @return A factor vector.
#' @keywords xtile quantiles cut
#' @examples
#' 
#' tertiles <- xtile(rnorm(1000),3)
#' table(tertiles)
#' 
#' @export xtile
xtile <- function(x, n) {
  cuts <- quantile(x, probs=seq(0,1,length=n+1))
  cut(x, breaks=cuts, include.lowest=TRUE)
}
