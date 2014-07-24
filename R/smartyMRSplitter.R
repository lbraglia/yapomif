#' Smarty Multiple Response Splitter
#'
#' This function split a variable containing Smarty's output for
#' a multiple response question in a dummy data.frame, to be
#' used with \code{\link{cbind}}.
#'
#' @param x variable to be splitted
#' @param categories character vector with, if NULL all
#' categories given in x will be used (in alphabetic order)
#' @export
smartyMRSplitter <- function(x, categories = NULL) {
  
  spl <- strsplit(x = x, split = "|", fixed = TRUE)
  xName <- deparse(substitute(x))
  if (is.null(categories)) {
    categories <- sort(unique(unlist(spl)))
    categories <- categories[!is.na(categories)]
  }

  res <- as.data.frame(do.call(rbind, lapply(spl, function(x) as.integer(categories %in% x))))
  res$count <- apply(res, 1, sum)
  names(res) <- paste(xName,
                      c(gsub(" +", "_", categories), "count"), 
                      sep = "_")
  res

}

