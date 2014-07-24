#' Preprocess data.frame variable names
#'
#' Function to preprocess variable names useful for a data.frame.
#' This function was created to make automatic variable name
#' creation from Excel files obtained by other.
#'
#' @param varnames names of a data.frame
#' @param trim character length of trimming. If \code{NULL}
#' (default) trimming is disabled.
#' @export
preprocessVarnames <- function(varnames, trim=NULL) {

    if (is.null(varnames)) stop("names(db) needed")

    ## tolower
    varnames <- tolower(varnames)	
    if (any(duplicated(varnames))) 
        stop("lower case make them not unique; ",
             "are they such, in the original source?") 
    
    ## Multiple spaces (eg Excel-from) to unique underscore
    varnames <- gsub(" +","_", varnames)

    ## change some math operators to equivalent words ("-" excluded)
    varnames <- gsub("\\/","_frac_", varnames)
    varnames <- gsub("\\*","_per_", varnames)
    varnames <- gsub("\\+","_plus_", varnames)

    ## dot to underscore
    varnames <- gsub("\\.","_", varnames)

    ## rm accents
    varnames <- rmAccents(varnames)
    
    ## rm unprintable chars
    varnames <- rmUnprintableChars(varnames)
    
    ## rm other annoying chars
    varnames <- gsub("'", "", varnames)
    varnames <- gsub("\U00B0", "", varnames)

    ## Trim to length specified
    if (!is.null(trim))
        varnames <- strtrim(varnames, trim)
    
    return(varnames)	
    
    
}	
