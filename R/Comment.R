#' Print a message inside an ascii border
#'
#' Print a message inside an ascii border; useful for working
#' with \code{\link{sink}}, to leave messages in the output.
#'
#' @param ... character vectors (pasted) forming the message to
#' be printed 
#' @param border character used for header printing
#' @param wrap where to wrap
#' @export 
Comment <- function(... ,
                    border = "-",
                    wrap = options()$width
                    ) {

  msg <- do.call( paste, list(...) )
  
  if (nchar(msg) > wrap) {
    ## wrap it
    msg <- paste(strwrap(msg, width = wrap), collapse = "\n")
    border <- paste(rep(border, wrap),  collapse = "")
  }
  else {
    border <- paste(c(rep(border, nchar(msg)), "\n"), collapse = "")
  }

  cat("\n", border, sep="")
  cat(msg, "\n")
  cat(border, "\n")

}



  


