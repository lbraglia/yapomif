#' Group progressive id creator
#'
#' Starting from a vector of group id, this function creates a progressive
#' id inside each group.  It uses C for efficiency
#' 
#' @param group a group vector
#' @examples
#' set.seed(1)
#'
#' ## One factor id
#' x <- sample(c(rep("A",5), rep("C",3), rep("B",2), rep(NA,2)))
#' data.frame(group = x, id = groupProgID(x) )
#'
#' ## Two factors id
#' y <- sample(gl(2,6, labels = c("C","D")))
#' data.frame(group1 = x, group2 = y, id = groupProgID(interaction(x,y)) )
#'
#' ## example of sorting
#' db <- data.frame("group" = sample(gl(2,5)), "b" = Sys.Date() + 1:10)
#' db <- db[order( - as.integer(db$group), db$b),]
#' data.frame(db, id = groupProgID(db$group))
#' @export
groupProgID <- function(group) {

  ## make NA as a valid level for low-level operations
  group2 <- as.integer(factor(group, exclude = NULL))
  res <- .Call("groupProgID_slave",
               group2,
               max(group2, na.rm=TRUE),
               package = "yapomf")
  ## handling NA
  res[is.na(group)] <- NA
  res

}
