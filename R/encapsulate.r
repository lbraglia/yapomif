#' Encapsulate text between margins.
#' 
#' Encapsulate text between margins.
#' 
#' 
#' @param s string to be encapsulated
#' @param start left margin
#' @param last right margin
#' @param sep separator between string and margins
#' @return An encapsulated string.
#' @keywords graph x11 window
#' @examples
#' 
#' encapsulate(letters)
#' encapsulate(letters, sep=" ")[1:10]
#' 
#' 
#' @export encapsulate
encapsulate <- function(s,start="[", last="]",sep="")
    sprintf( paste(rep("%s",3), collapse=sep), start, s, last)

## Funzione utile per incapsulare del testo all'interno
## di marker, di default [ e ] utile per le label dei
## factor

## Esempi
## encapsulate(letters)
## encapsulate(letters, sep=" ")
## encapsulate(letters, start="#")
## encapsulate(letters, start="#", sep=" ")


