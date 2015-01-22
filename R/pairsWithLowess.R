#' Scatterplot Matrices with a lowess smoother
#' 
#' 
#' A Matrix of Scatterplot with a lowess smoother is produced.
#' 
#' 
#' @param data data.frame
#' @return Plot
#' @export 
pairsWithLowess <- function(data) {
  pairs(data,
        upper.panel = function(x,y) {
          points(x,y)
          lines(lowess(x,y), col = 2)
        },
        lower.panel = function(x,y) {
          points(x,y, type="n")
        }
        )
}					
