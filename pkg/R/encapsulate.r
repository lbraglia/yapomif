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


