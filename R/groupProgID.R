#'
#' Group progressive id creator
#'
#' Starting from a vector of group id (not necessarily ordered),
#' this function creates a progressive id inside each group.
#' 
#' @note To be implemented in C/C++ 
#'
#' @usage groupProgID(group) 
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
	
	# Progress counter
	counters <- rep(0, length(ids) )
	names(counters) <- ids
	
	# Results vector
	results <- rep(NA, length(group))
	
	for( val in 1:length(group) ) {
		counters[group[val]] <- counters[group[val]] +1
		results[val] <- counters[group[val]]
	}

	results
}


## group.prog.id2 <- function(group.id) {
## da testare questa seconda versione
## ----------------------------------
## 	stopifnot(all(sort(group.id)== group.id))

## 	counts <- as.numeric(table(group.id))
## 	unlist(lapply(counts, seq_len))

## }
