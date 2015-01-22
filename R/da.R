#' Calculate diagnostic accuracy measures for binary measures (test, reference
#' standard)
#' 
#' 
#' Calculate diagnostic accuracy measures for binary measures (test, reference
#' standard)
#' 
#' 
#' @param test Test (dichotomic)
#' @param refstd Reference standard (dichotomic)
#' @param alpha Type I error (for two sided confidence interval)
#' @param round.dig Rounding digits
#' @param positive.first Display positive test and reference standard in first
#' row column
#' @param ppv.npv.prev Prevalence adopted for ppv and npv confidence interval
#' (if NULL estimated from sample)
#' @param ppv.npv.force.unadj.est Force unadjusted ppv npv estimates (and
#' confidence interval)
#' @return Diagnostic accuracy table and statistics with confidence interval.
#' @examples
#' 
#' 
#' ## Example CASS pag 22 Pepe
#' db <- dadb(tn=327 , fn=208 ,fp =115 , tp=815)
#' with(db, da(test=test, refstd=refstd))
#' 
#' ## Example Alzheimer mercaldo
#' db <- dadb(tn=288 , fn=178 ,fp =87 , tp=240)
#' with(db, da(test=test, refstd=refstd, ppv.npv.prev=.03))
#' 
#' 
#' @export
da <- function(test=NULL, refstd=NULL,
               alpha=.05, round.dig=4,
               ## the table print, should put + before - 
               ## (both for test and refst, 
               ## the factor put - first and then +)
               positive.first=TRUE,
               ## Prevalenza per il calcolo dei ci di mercaldo per ppv npv
               ## Se NULL viene utilizzata quella disponibile dal campione
               ppv.npv.prev=NULL,
               ppv.npv.force.unadj.est=FALSE
               ) {
    
    ## For npv ppv confidence intervals
    ## require(bdpv)
    
    ok.input <- (is.factor(test) | is.logical(test)) & 
        (is.factor(refstd) | is.logical(refstd)) &
            (is.logical(positive.first)) &
                (is.null(ppv.npv.prev) | is.numeric(ppv.npv.prev))
    
    ## testa l'immissione
    if (!(ok.input)) {
        stop("'test' and/or 'refstd' missing or not (factor |logical).\n",
             "OR positive.first not logical\n",
             "OR 'ppv.npv.prev'  not (null|numeric)"
             )
    }
		
    ## Mette a posto i valori logici (trasformandoli in factor)
    if (is.logical(test)) {
        test <- factor(test, levels=c(FALSE, TRUE))
    } 
    if (is.logical(refstd)) {
        refstd <- factor(refstd, levels=c(FALSE, TRUE))
    }

    ## Controlla che sia test che refstd abbiano 2 livelli
    if ( ! ((nlevels(test) == 2L) & (nlevels(refstd) == 2L)) ) {
        stop("nlevels must be 2 for both test and refstd")
    }
	
    ## ##############   TABLE ##########################
    
    ## Gestione delle label della tabella per la stampa
    tab <- table(test,refstd)
    tn <- tab[1,1]
    tp <- tab[2,2]
    fn <- tab[1,2]
    fp <- tab[2,1]
    
    ## positive first handling
    ## ora che ho estratto i dati da una forma tipica dell'elaborazione 
    ## li metto in forma tipica per la stampa, se desiderato
    tab.positive.first <- matrix(c(tab[2,2],tab[2,1], 
                                   tab[1,2],tab[1,1]),
                                 ncol=2, byrow=T,
                                 dimnames=list("test"=rev(levels(test)),
                                     "refstd"=rev(levels(refstd)) 
                                     ))
    class(tab.positive.first) <- class(tab)

    ## Table for results depends on positive.first
    tab.for.results <- if (positive.first) {
        tab.positive.first
    } else {
        tab
    }

    ## ##############   STATS   ##########################
	
    n.diseased <- sum(tab[,2])
    n.non.diseased <- sum(tab[,1])
    n.positive <- sum(tab[2,])
    n.negative <- sum(tab[1,])
    n <- sum(tab)
    ## serve per il calcolo degli intervalli di confidenza di ppv npv
    z <- qnorm(1-alpha/2)

    ## prevalence
    prevalence <- n.diseased / n
    prevalence.ci <- (binom.test( n.diseased, n, conf.level = 1 - alpha )$conf.int)[1:2]

    ### Sensitivity
    sensitivity <- tp/n.diseased
    sensitivity.ci <- (binom.test( tp, n.diseased, conf.level = 1 - alpha )$conf.int)[1:2]
    
    ## Specificity
    specificity <- tn/n.non.diseased
    specificity.ci <- (binom.test( tn, n.non.diseased, conf.level = 1 - alpha )$conf.int)[1:2]

    ## Accuracy
    accuracy <- (tn+tp)/ n
    accuracy.ci <- (binom.test( tn+tp, n, conf.level = 1 - alpha )$conf.int)[1:2]
    
    
    ## Odd ratio diagnostico
    ## or <- (sensitivity/(1-sensitivity))/ ((1-specificity)/specificity)
    or <- (tp * tn)/ (fp*fn)
    or.se <- sqrt(1/tp + 1/tn + 1/fp + 1/fn)
    or.ci <- exp( c(log(or) - z*or.se, log(or) + z*or.se) )
    
    ## Youden index
    youden <- sensitivity + specificity - 1
    
    
    ## Predictive value: for more stuff check out library(bdpv)
    ## -------------------------
    ## Intervalli di confidenza di npv npp adottando la formula
    ## di mercaldo (pag 108 di zhou e stat in medicine 2007) che
    ## permette di specificare una prevalenza diversa da quella
    ## riscontrata nel campione.  Tutte e due le implementazioni
    ## del paper disponibili.  Prima la standard logit, qui
    ## sotto, poi la adjusted logit

    if( is.null(ppv.npv.prev) ) {
        ## Se l'utente non ha specificato un valore di prevalenza, 
        ## prendere quello del campione
        prev <- prevalence
    } else if (is.numeric(ppv.npv.prev) & (ppv.npv.prev>=0 &
                                           ppv.npv.prev<=1 )){
        ## Se invece l'ha specificato, posto che sia ragionevo
        prev <- ppv.npv.prev
    } else {
        stop("ppv.npv.prev must be NULL or numeric [0,1]")		
    }	
	
    if (all(tab>0) | ( any(tab==0) & (ppv.npv.force.unadj.est==TRUE)) ) {
        ## PPV NPV standard logit estimates (mercaldo)
        
        ## PPV standard logit
        ppv <- (sensitivity*prev)/((sensitivity*prev) + (1-specificity)*(1-prev) )
        logit.ppv <- log((sensitivity * prev) /
                         ((1-specificity)*(1-prev)))
        var.logit.ppv <- ((1-sensitivity)/sensitivity)*(1/n.diseased) +
            (specificity/(1-specificity))*(1/n.non.diseased)

        ppv.wald.low <- exp(logit.ppv - z*sqrt(var.logit.ppv))/
            (1 + exp(logit.ppv - z*sqrt(var.logit.ppv)))
        ppv.wald.up <- exp(logit.ppv + z*sqrt(var.logit.ppv))/
            (1 + exp(logit.ppv + z*sqrt(var.logit.ppv)))
        ppv.ci <- c(ppv.wald.low, ppv.wald.up)

        ## NPV standard logit
        npv <- 	(specificity*(1- prev))/
            (  (1-sensitivity)*(prev)  +  specificity*(1- prev) )
        
        logit.npv <- log((specificity * (1-prev)) /
                         ((1-sensitivity)*(prev)))
        var.logit.npv <- (sensitivity/(1-sensitivity))*(1/n.diseased) +
            ((1-specificity)/specificity)*(1/n.non.diseased)

        npv.wald.low <- exp(logit.npv - z*sqrt(var.logit.npv))/
            (1 + exp(logit.npv - z*sqrt(var.logit.npv)))
        npv.wald.up <- exp(logit.npv + z*sqrt(var.logit.npv))/
            (1 + exp(logit.npv + z*sqrt(var.logit.npv)))
        npv.ci <- c(npv.wald.low, npv.wald.up)
        
    } else {
	
        ## PPV NPV adjusted logit estimates (mercaldo)
        xmat <- unname(as.matrix(tab.positive.first))
        class(xmat) <- "matrix"
        pv.tmp <- bdpv::BDtest(xmat = xmat, 
                         pr=prev, 
                         conf.level = 1 - alpha)[["PPVNPVDAT"]]
        ppv.row <- row.names(pv.tmp) %in% "PPV"
        npv.row <- row.names(pv.tmp) %in% "NPV"
        ppv <- pv.tmp[ppv.row,1]
        ppv.ci <-  unlist(unname(c(pv.tmp[ppv.row, 3:4])))
        npv <- pv.tmp[npv.row,1]
        npv.ci <- unlist(unname(c(pv.tmp[npv.row, 3:4])))
	
    }
	
    ## Statistiche complessive
    stat <- as.data.frame( rbind(
        c(prevalence, prevalence.ci),
        c(sensitivity, sensitivity.ci),
        c(specificity, specificity.ci),
        c(accuracy, accuracy.ci),
        c(ppv, ppv.ci),
        c(npv, npv.ci),
        c(or, or.ci),
        c(youden, rep(NA,2))
	))
    
    stat <- round(stat,round.dig)
    names(stat) <- c("Est","Low.CI", "Up.CI")
    stat$stat <- c("Prevalence","Sensitivity",
                   "Specificity","Accuracy",
                   "PPV","NPV","OR","Youden")
    ## riordino le variabili
    stat <- stat[c(4,1:3)]

    
    ## #####################   OUTPUT   ##########################
		
    ## return results
    list("table"= addmargins(tab.for.results), 
         "stats"= stat)
	
    ## Example CASS pag 22 pepe
    ## db <- dadb(tn=327 , fn=208 ,fp =115 , tp=815)
    ## with(db, da2(test=test, refstd=refstd))
    
    ## Example di mercaldo su alzheimer
    ## db <- dadb(tn=288 , fn=178 ,fp =87 , tp=240)
    ## with(db, da2(test=test, refstd=refstd, ppv.npv.prev=.03))

}







## # diagnostic accuracy common indicators
## da.old <- function(test=NULL, refstd=NULL,
## 				alpha=.05, round.dig=4,
## 				# the table print, should put + before - 
## 				# (both for test and refst, 
## 				# the factor put - first and then +)
## 				positive.first=TRUE,
## 				# Prevalenza per il calcolo dei ci di mercaldo per ppv npv
## 				# Se NULL viene utilizzata quella disponibile dal campione
## 				ppv.npv.prev=NULL
## 				) 
## {
	
## 	ok.input <- (is.factor(test) | is.logical(test)) & 
## 				(is.factor(refstd) | is.logical(refstd)) &
## 				(is.logical(positive.first)) &
## 				(is.null(ppv.npv.prev) | is.numeric(ppv.npv.prev))
	
## 	# testa l'immissione
## 	if (!(ok.input)) {
##         stop(	"'test' and/or 'refstd' missing or not (factor |logical).\n",
## 			 	"OR positive.first not logical\n",
## 				"OR 'ppv.npv.prev'  not (null|numeric)"
## 				)
## 	}
		
## 	# Mette a posto i valori logici (trasformandoli in factor)
## 	if (is.logical(test)) {
## 		test <- factor(test, levels=c(FALSE, TRUE))
## 	} 
## 	if (is.logical(refstd)) {
## 		refstd <- factor(refstd, levels=c(FALSE, TRUE))
## 	}


## 	# Controlla che sia test che refstd abbiano 2 livelli
## 	if ( ! ((nlevels(test) == 2L) & (nlevels(refstd) == 2L)) ) {
## 		stop("nlevels must be 2 for both test and refstd")
## 	}
	
## 	################   TABLE ##########################
	
## 	# Gestione delle label della tabella per la stampa
##     tab <- table(test,refstd)
##     tn <- tab[1,1]
##     tp <- tab[2,2]
##     fn <- tab[1,2]
##     fp <- tab[2,1]

## 	# positive first handling
## 	# ora che ho estratto i dati da una forma tipica dell'elaborazione 
## 	# li metto in forma tipica per la stampa, se desiderato
## 	tab.positive.first <- matrix(c(tab[2,2],tab[2,1], 
## 							tab[1,2],tab[1,1]),
## 							ncol=2,	byrow=T,
## 					dimnames=list("test"=rev(levels(test)),
## 								"refstd"=rev(levels(refstd)) 
## 					))
## 	class(tab.positive.first) <- class(tab)

## 	# Table for results depends on positive.first
## 	tab.for.results <- if (positive.first) {
## 		tab.positive.first
## 	} else {
## 		tab
## 	}

## 	################   STATS   ##########################

	
## 	n.diseased <- sum(tab[,2])
## 	n.non.diseased <- sum(tab[,1])
## 	n.positive <- sum(tab[2,])
## 	n.negative <- sum(tab[1,])
## 	n <- sum(tab)
## 	# serve per il calcolo degli intervalli di confidenza di ppv npv
## 	z <- qnorm(1-alpha/2)

	
##     # prevalence
##     prevalence <- n.diseased / n
##     prevalence.ci <- (binom.test( n.diseased, n, conf.level = 1 - alpha )$conf.int)[1:2]

	
##     ## Sensitivity
##     sensitivity <- tp/n.diseased
##     sensitivity.ci <- (binom.test( tp, n.diseased, conf.level = 1 - alpha )$conf.int)[1:2]

##     ## Specificity
##     specificity <- tn/n.non.diseased
##     specificity.ci <- (binom.test( tn, n.non.diseased, conf.level = 1 - alpha )$conf.int)[1:2]

##     ## Accuracy
##     accuracy <- (tn+tp)/ n
##     accuracy.ci <- (binom.test( tn+tp, n, conf.level = 1 - alpha )$conf.int)[1:2]
    
	
## 	# Odd ratio diagnostico
## 	# or <- (sensitivity/(1-sensitivity))/ ((1-specificity)/specificity)
## 	or <- (tp * tn)/ (fp*fn)
## 	or.se <- sqrt(1/tp + 1/tn + 1/fp + 1/fn)
## 	or.ci <- exp( c(log(or) - z*or.se, log(or) + z*or.se) )
	
## 	# Youden index
## 	youden <- sensitivity + specificity - 1

	
## 	## Positive predictive value
## 	## -------------------------
## 	## Intervalli di confidenza di npv npp adottando la 
## 	## formula di mercaldo (pag 108 di zhou e stat in medicine 2007)
## 	## che permette di specificare una prevalenza diversa da quella riscontrata
## 	## nel campione.
## 	## Tutte e due le implementazioni del paper  disponibili.
## 	## Prima la standard logit, qui sotto, poi la adjusted logit

## 	if( is.null(ppv.npv.prev) ) {
## 		# Se l'utente non ha specificato un valore di prevalenza, 
## 		# prendere quello del campione
## 		prev <- prevalence
## 	} else if (is.numeric(ppv.npv.prev) & (ppv.npv.prev>=0 & ppv.npv.prev<=1 )){
## 		# Se invece l'ha specificato, posto che sia ragionevo
## 		prev <- ppv.npv.prev
## 	} else {
## 		stop("ppv.npv.prev must be NULL or numeric [0,1]")		
## 	}	

## 	#### PPV standard logit
##     ppv <- (sensitivity*prev)/((sensitivity*prev) + (1-specificity)*(1-prev) )
    	
## 	logit.ppv <- log((sensitivity * prev) /
## 	                 ((1-specificity)*(1-prev)))
## 	var.logit.ppv <- ((1-sensitivity)/sensitivity)*(1/n.diseased) +
## 					 (specificity/(1-specificity))*(1/n.non.diseased)
## 	ppv.wald.low <- exp(logit.ppv - z*sqrt(var.logit.ppv))/
## 					(1 + exp(logit.ppv - z*sqrt(var.logit.ppv)))
## 	ppv.wald.up <- exp(logit.ppv + z*sqrt(var.logit.ppv))/
## 					(1 + exp(logit.ppv + z*sqrt(var.logit.ppv)))

## 	#### NPV standard logit
##     npv <- 	(specificity*(1- prev))/
## 			(  (1-sensitivity)*(prev)  +  specificity*(1- prev) )

## 	logit.npv <- log((specificity * (1-prev)) /
## 	                 ((1-sensitivity)*(prev)))
## 	var.logit.npv <- (sensitivity/(1-sensitivity))*(1/n.diseased) +
## 					 ((1-specificity)/specificity)*(1/n.non.diseased)

## 	npv.wald.low <- exp(logit.npv - z*sqrt(var.logit.npv))/
## 					(1 + exp(logit.npv - z*sqrt(var.logit.npv)))
## 	npv.wald.up <- exp(logit.npv + z*sqrt(var.logit.npv))/
## 					(1 + exp(logit.npv + z*sqrt(var.logit.npv)))
	
	
	
	
	
## 	# scelta dell'intervallo di confidenza da mostrare
## 	ppv.ci <- c(ppv.wald.low, ppv.wald.up)
## 	npv.ci <- c(npv.wald.low, npv.wald.up)
	
	
## 	# Statistiche complessive
## 	stat <- as.data.frame( rbind(
## 		c(prevalence, prevalence.ci),
## 		c(sensitivity, sensitivity.ci),
## 		c(specificity, specificity.ci),
## 		c(accuracy, accuracy.ci),
## 		c(ppv, ppv.ci),
## 		c(npv, npv.ci),
## 		c(or, or.ci),
## 		c(youden, rep(NA,2))
## 	))
	
## 	stat <- round(stat,round.dig)
## 	names(stat) <- c("Est","Low.CI", "Up.CI")
##     stat$stat <- c("Prevalence","Sensitivity",
## 					"Specificity","Accuracy",
## 					"PPV","NPV","OR","Youden")
## 	# riordino le variabili
## 	stat <- stat[c(4,1:3)]

	
## 	################   OUTPUT   ##########################
		
## 	# return results
##     list("table"= addmargins(tab.for.results), 
## 		"stats"= stat)
	
## 	## Example CASS pag 22 pepe
##     ## db <- dadb(tn=327 , fn=208 ,fp =115 , tp=815)
## 	## with(db, da2(test=test, refstd=refstd))
	
## 	## Example di mercaldo su alzheimer
##     ## db <- dadb(tn=288 , fn=178 ,fp =87 , tp=240)
## 	## with(db, da2(test=test, refstd=refstd, ppv.npv.prev=.03))
	

## }

