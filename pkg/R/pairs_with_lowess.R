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
