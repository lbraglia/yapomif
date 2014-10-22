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
#' test2 <- test
#' test2$surname <- c("doe", "foo", "bar")
#' test2 <- test2[c("name","surname","x","y","z")]
#'
#' test
#' NAreport(test, "name")
#'
#' test2
#' NAreport(test2, c("name", "surname"))
#' @export
NAreport <- function(x, idvar = NULL) {
  ## input checking
  if (!is.data.frame(x))
    stop("x must be a data.frame")
  if (! (is.character(idvar) & length(idvar)>0L) )
    stop("idvar must be a positive length character vector")
  if (anyDuplicated(x[,idvar]))
    stop("'idvar' must identify rows uniquely")
  ## multiple ids handling
  if (length(idvar) == 1L)
    ID <-  x[,idvar]
  else {
    ID <- do.call("paste", c(x[,idvar], sep = "\r"))
    idDF <- cbind(x[,idvar], ID)
  }
  ## splitting per id (one line per list element)
  splitted <- split(x[, !(names(x) %in% idvar)], f = factor(ID))
  ## Missing variable per units
  nas <- lapply(splitted, function(x) names(x)[is.na(x)])
  ## Creating a similar data structure for id
  ids <- mapply(FUN = rep, names(nas), lapply(nas, length))
  ## putting all together
  res <- as.data.frame(do.call("rbind", mapply(cbind, ids, nas)))
  ## back to original id (not pasted one) for multiple id columns data.frame
  if (length(idvar) > 1L){
    res <- merge(idDF, res, all.x = TRUE, by.x = "ID", by.y = "V1")
    res$ID <- NULL
  }
  names(res) <- c(idvar, "variable")
  return(res)
}
