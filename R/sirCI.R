#' Exact CI (and estimate) for SIR/SMR
#'
#' Exact CI (and estimate) for SIR/SMR
#' @param O observed cases
#' @param E expected cases
#' @param conf.level confidence level for the returned confidence interval
#' (two sided)
#' @seealso Guidelines for Using
#' confidence intervals for public healt assessment" - Washington State
#' Department of Health, July 13, 2012
#' @examples
#' ## FDK Liddel, "Simple exact analysis of the standardised mortality
#' ## ratio", Journal of Epidemiology and Community Health 1984, 38, 85-88
#' x <- data.frame(D = c(8,23,210), E=c(3.59,17.83,180), cl = c(0.9,0.95,0.95))
#' t(apply(x, 1, function(x) sirCI(x[1], x[2], x[3])))
#' @export
sirCI <- function(O, E, conf.level = 0.95){
  
  res <- poisson.test(x = O, conf.level = conf.level)
  res <- unname(c(res$conf.int[1], res$estimate, res$conf.int[2]))/E
  names(res) <- c("Lower.Ci", "Estimate", "Upper.Ci")
  return(res)
}
