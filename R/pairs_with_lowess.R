#' Scatterplot Matrices with a lowess smoother
#' 
#' 
#' A Matrix of Scatterplot with a lowess smoother is produced.
#' 
#' 
#' @usage pairs_with_lowess(data)
#' @param data data.frame
#' @return Plot
#' @keywords pairs lowess scatterplot matrix
#' @export pairs_with_lowess
pairs_with_lowess <- function(data) {
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
