#' Remove accents from a character vector
#'
#' Remove accents from a character.
#'
#' @param string a character vector
#' @export
rmAccents <- function(string) {
    
    ## a
    string <- gsub("[\U00E0|\U00E1|\U00E2|\U00E3|\U00E4|\U00E5]",
                   "\U0061", string)
    
    ## e
    string <- gsub("[\U00E8|\U00E9|\U00EA|\U00EB]",
                   "\U0065", string)      
    ## i
    string <- gsub("[\U00EC|\U00ED|\U00EE|\U00EF]",
                   "\U0069", string)
    ## o
    string <- gsub("[\U00F2|\U00F3|\U00F4|\U00F5|\U00F6]",
                   "\u006F", string)     
    ## u
    string <- gsub("[\U00F9|\U00FA|\U00FB|\U00FC]",
                   "\u0075", string)     
    
    return(string)
    
}
