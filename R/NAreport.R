#' Report NA variables per case
#'
#' Report NA variables per case
#' @param x data.frame to be analyzed
#' @param idvar variable to be used as id
#' @examples
#' test <- data.frame(
#'  name =  c("john","mary","gregor"),
#'  x = c(1,NA,NA),
#'  y = c(NA,1,1),
#'  z = c(1,1,NA))
#' 
#' NAreport(test, idvar = "name")
#' @export
NAreport <- function(x, idvar) {
  splitted <- split(x[, !(names(x) %in% idvar)], f = factor(x[,idvar]))
  nas <- lapply(splitted, function(x) names(x)[is.na(x)])
  ids <- mapply(FUN = rep, names(nas), lapply(nas, length))
  res <- as.data.frame(do.call("rbind", mapply(cbind, ids, nas)))
  names(res) <- c(idvar, "variable")
  return(res)
}


