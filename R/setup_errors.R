#' Descriptive statistics for setup errors in radiotherapy
#' 
#' Compute standard M, Sigma and sigma values,  according to van Herk
#' (2004).
#' @param x vector of numeric setup error
#' @param group patient id vector
#' @examples db <- data.frame(pt = factor(rep(1:4, each = 4)),
#'                            time = rep(1:4, 4),
#'                            y = c(
#'                            2,  1, 1, 1,
#'                            4, -2, 2, 0,
#'                            1, -1, 2, 2,
#'                            3, -3, -2, 1
#'                            ))
#' setup_errors(db$y, db$pt)
#' @references Marcel van Herk, Errors and Margins in Radiotherapy,
#'                            Seminars in Radiation Oncology, Vol 14, No 1
#'                            (January), 2004 
#' @export
setup_errors <- function(x, group){

  rval <- list()
  means <- tapply(x, group, mean, na.rm = TRUE)
  vars <- tapply(x, group, stats::var, na.rm = TRUE)

  ## Systematic error
  rval$M <- mean(means)
  ## Systematic error variability
  rval$Sigma <- sd(means)
  ## Random error variability (RMS)
  rval$sigma <- sqrt(mean(vars))

  ## Return
  unlist(rval)
  
}


