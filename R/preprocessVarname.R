#' Preprocess data.frame variable names
#'
#' Function to preprocess variable names useful for a data.frame.
#' This function was created to make automatic variable name
#' creation from Excel files obtained by other.
#'
#' @param varnames names of a data.frame (or the data.frame itself)
#' @param trim character length of trimming. If \code{NULL}
#' (default) trimming is disabled.
#' @export
preprocessVarnames <- function(varnames = NULL, trim = NULL) {

    ## handling special cases
    if (is.data.frame(varnames))
        varnames <- names(varnames)
    if (! is.character(varnames))
        stop("Names needed as a vector of character")

    ## tolower
    varnames <- tolower(varnames)	
    if (any(duplicated(varnames))) 
        stop("lower case make them not unique; ",
             "are they such, in the original source?") 
    
    ## rm parenthesis
    varnames <- gsub('[\\(\\)]', '', varnames)

    ## Multiple spaces (eg Excel-from) to unique underscore
    varnames <- gsub(" +","_", varnames)

    ## change some math operators to equivalent words ("-" excluded)
    varnames <- gsub("-","_", varnames)
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

    ## remove starting or ending '_'
    varnames <- gsub("_+$", "", varnames)
    varnames <- gsub("^_+", "", varnames)
    ## unique remaining multiple/near '_'
    varnames <- gsub("_+","_", varnames)
    
    ## Trim to length specified
    if (!is.null(trim))
        varnames <- strtrim(varnames, trim)
    
    return(varnames)	
    
    
}	
