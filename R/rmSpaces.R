#' Remove spaces from a character vector
#'
#' This function removes spaces (all leading and trailing ones,
#' and uniques in-between) from a character vector.
#'
#' @param string a character vector 
#' @export
#' @examples
#' test <- c("  test ", "  mediterranean  sea  ")
#' rmSpaces(test)
rmSpaces <- function(string) {
    ## Starting " "
    string <- gsub("[[:space:]]*$","", string, perl=T)
    ## Ending " "
    string <- gsub("^[[:space:]]*","", string, perl=T)
    ## 
    gsub("[[:space:]]+"," ", string, perl=T)
}

