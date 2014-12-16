#'
#' Group progressive id creator from unordered id vector
#'
#' Starting from a vector of group id (not necessarily ordered),
#' this function creates a progressive id inside each group.
#' 
#' @note To be implemented in C/C++ 
#'
#' @param group a group vector
#' @keywords group id
#' @examples
#' set.seed(1)
#' ## One factor id
#' x <- sample(gl(2,10, labels = c("A","B")))
#' cbind(x, groupProgID(x) )
#' ## Two factors id
#' y <- sample(gl(2,10, labels = c("C","D")))
#' cbind(x, y, groupProgID(interaction(x,y)) )
#' @export
groupProgID <- function(group) {

  group <- as.character(group)
  ids <- unique(group)
	
  ## Progress counter
  counters <- rep(0, length(ids) )
  names(counters) <- ids
  
  ## Results vector
  results <- rep(NA, length(group))
  
  for( val in 1:length(group) ) {
    counters[group[val]] <- counters[group[val]] +1
    results[val] <- counters[group[val]]
  }

  results
}


#'
#' Group progressive id creator from ordered id vector
#'
#' Starting from a vector of group id (that have to be ordered,
#' aka multiple elements from the same group must be in adjacent
#' rows, the group label is irrilevant), this function creates a
#' progressive id inside each group.
#' 
#' @param group a group vector
#' @keywords group id
#' @examples
#' set.seed(1)
#' ## One factor id
#' x <- c("A", rep("C",3), rep("B",2))
#' cbind(x, groupProgID2(x) )
#' ## example of sorting
#' db <- data.frame("group" = sample(gl(2,5)), "b" = Sys.Date() + 1:10)
#' (db <- db[order(- as.numeric(db$group), db$b),])
#' cbind(db, "seq" = groupProgID2(db$group))
#' @export
groupProgID2 <- function(group) {
  if (! is.factor(group))
    group <- as.integer(factor(group, labels = unique(group)))
  else
    group <- as.integer(group)
  counts <- as.numeric(table(group))
  unlist(lapply(counts, seq_len))

}
