#' perform Chi-square or Fisher test as appropriate
#'
#' perform Chi-square or Fisher test as appropriate. The function uses
#' \code{\link{fisherNeeded}} to decide whether to perform chi or fisher
#' test.
#'
#' @param x first variable
#' @param y second variable
#' @export
chi_or_fisher_test <- function(x, y){

  tab <- table(x, y)
  if(fisherNeeded(x, y))
    fisher.test(tab)
  else
    chisq.test(tab)

}


  
