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
#' Seminars in Radiation Oncology, Vol 14, No 1 (January), 2004
#' @references Joep Stroom and Ben Heijmen, Geometrical uncertainties,
#' radiotherapy planning margins, and the ICRU-62 report, Radiotherapy and
#' Oncology 64 (2002) 75-83
#' 
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
  ## PTV - ICRU 62
  rval$ptv_icru <- sqrt(rval$Sigma^2 + rval$sigma^2)
  ## PTV - Stroom
  rval$ptv_stroom <- rval$Sigma * 2 + rval$sigma * 0.7
  ## PTV - Van Herk
  rval$ptv_van_herk <- rval$Sigma * 2.5 + rval$sigma * 0.7
  
  ## Return
  unlist(rval)
  
}


