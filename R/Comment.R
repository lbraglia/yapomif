#' Print a message inside an Ascii border
#'
#' Print a message inside an Ascii border; useful for working
#' with \code{\link{sink}}, to leave messages in the output.
#'
#' @param msg character vectors (pasted) forming the message to
#' be printed 
#' @param border character used for header printing
#' @param wrap where to wrap
#' @export 
Comment <- function(msg ,
                    border = "-",
                    wrap = 0.9 * getOption("width")
                    ) {


  msg <- strwrap(msg, wrap)
  maxLen <- max(nchar(msg))
  msg <- paste(msg , collapse = "\n")
  border <- paste(rep(border, maxLen),  collapse = "")

  cat("\n", border, "\n", sep="")
  cat(msg)
  cat("\n", border, "\n", sep="")

}

  


