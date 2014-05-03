censor_at <- function(time=NULL, status=NULL, censor.time=NULL) {
    ## Funzione per tagliare un tempo all'evento ad un certo momento
    if (sum(sapply(list(time, status, censor.time), is.null))>0) {
        stop("time, status and censor.time needed")
    }

    ## Se il  tempo a cui si vuol tagliare è maggiore del tempo rilevato, 
    ## non fare nulla e restituisci la coppia time status così come sono
    
    ## Se invece è inferiore del tempo rilevato (time):
    ## - nel caso il paziente non abbia avuto l'evento al tempo rilevato, non 
    ##   l'ha avuto neanche al tempo precedente, pertanto bisogna
    ##   portare indietro 
    ##   il tempo lasciando stare la variabile indicatrice
    ## - nel caso il paziente abbia avuto l'evento al tempo t, si
    ##   ipotizza che a  t-1 non l'avesse (ovvero che un evento si
    ##   verifica nel giorno in cui è registrato),  
    ##   per cui si tratta di porre a 0 la variabile indicatrice
    ##   e di riportare indietro il conteggio dei giorni al
    ##   cutoff
    ## In sostanza in entrambi i casi dummy=0 e time=censor.time

    subst <- function(t,s,c) {
        if (t <= c) {
            c(t,s)
        } else {
            c(c,0)
        }
    }
	
    df <- data.frame(time, status)
    res <- data.frame(t(apply(df, 1,
                              function(x) subst(t=x[1], s=x[2],
                                                c=censor.time)
                              ))) 
    names(res) <- paste(c("time", "status"),
                        paste("c", censor.time ,sep=""),
                        sep=".")
    return(res)
    
    
}




