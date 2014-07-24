#' Plots an 'enhanced' Kaplan-Meier plot, with base graphics package.
#' 
#' 
#' This function plots a Kaplan-Meier plot.
#' 
#' 
#' @usage km(time=NULL, status=NULL, strata=NULL,
#' time.unit=c("days","weeks","months","years"), time.by=NULL, main="",
#' ylab=NULL, xlab=NULL, xlim=NULL, ylim=c(0,1), conf.int=NULL,
#' test=c("logr","hr","both","no"), plot.n.at.risk=TRUE, gr.legend=NULL, ...  )
#' @param time survival time variable
#' @param status survival indicator variable
#' @param strata Stratifying variable (optional)
#' @param time.unit Time unit of x axis
#' @param time.by Time step x axis (in days)
#' @param main Graph main title
#' @param ylab Y-axis label.
#' @param xlab X-axis label. If NULL a suitable default based on time.unit will
#' be provided
#' @param xlim X-axis limit. If NULL a suitable default based on time.unit will
#' be provided
#' @param ylim Y-axis limit. Default to c(0,1)
#' be provided
#' @param conf.int logical ... Plot confidence intervall? If NULL confidence
#' interval are plotted only if strata has two or more levels
#' @param test tests: 'no'=don't plot tests, 'logr'=logranktest, 'hr'=hazratio,
#' 'both'=both
#' @param plot.n.at.risk Logical value: plot number at risk?
#' @param gr.legend Graph command to add legend, as string
#' @param ... Further \code{\link{lines.survfit}} parameters
#' @return The function plot the graph and return a list with
#' Laplan-Meier statistics
#' @keywords Kaplan Meier survival
#' @export km
km <- function(time=NULL,
               status=NULL,
               ## NULL = non stratified, otherwise stratifying variable
               strata=NULL,
               ## Time unit x axis
               time.unit=c("days","weeks","months","years"),
               ## Time step x axis (in days)
               time.by=NULL,
               ## Main title
               main="",
               ## Y axis label
               ylab=NULL,
               ## X axis nbel
               xlab=NULL,
               ## X axis limits
               xlim=NULL,
               ## Y axis limits
               ylim=c(0,1),
               ## PLot Confidence interval
               conf.int=NULL,
               ## Test: no=don't plot tests, logr=logranktest,
               ##		hr=hazratio,both=both
               test=c("logr","hr","both","no"),
               ##Plot number ad risk in the km
               plot.n.at.risk=TRUE,
               ##Graph command to add legend, as string
               gr.legend=NULL,
               ## Further lines.survfit params
               ...                     
               ){
    ## Occhio
    ## la funzione IPOTIZZA CHE I TEMPI ALL'EVENTO 
    ## SIANO MISURATI IN GIORNI. 
    ## Se misurati in modo differente (es settimane), 
    ## lasciare non specificato time.unit

    ## TODO
    ## - inserire l'opzione data come possibile
    ## - permettere al plot di incastrarsi in un mfrow specificato a monte
    ##   Attualmente possibile solo se plot.n.at.risk=F (perchè in tal
    ##   caso non si usa oma)
    ## - inserire la possibilità di visualizzare la mediana 
    ##   di sopravvivenza
    ## - sistemare il settore sinistro del grafico per permettere che 
    ##   stringhe molto lunghe (il nome dei gruppi, se lunghi) 
    ##   non vengano tagliate negli at risk 
    ## - inserire il parametro data come specificabile

    ## ## Libraries required
    ## require(survival)
    ## require(graphics)

    ## time status check (EXISTENCE)
    if (sum(sapply(list(status, time), is.null)) > 0) 
        stop("'status' and 'time' needed")

    ## time.by check (NULL or numeric)
    if(!(is.null(time.by) | is.numeric(time.by)))
        stop("'time.by' must be NULL or numeric")

    ## xlab ylab (NULL or CHAR)
    if (sum(sapply(list(ylab, xlab), 
                   function(x) (is.null(x) | is.character(x)) )) < 2) 
        stop("ylab and xlab have to be NULL or character")

    ## time.unit, main and test (CHAR)
    if (sum(sapply(list(time.unit, main,  test), 
                   function(x) !is.character(x))) > 0) 
        stop("time.unit, main, test have to be character")

    ## plot.n.at.risk (LOGICAL)
    if(!(is.logical(plot.n.at.risk) | is.numeric(plot.n.at.risk)))
        stop("'plot.n.at.risk' must be logical")

    ## CONF.INT (NULL OR LOGICAL)
    if(!(is.logical(conf.int) | is.null(conf.int)))
        stop("'conf.int' must be NULL or logical")
    
    ## xlim (NULL or numeric vector of length 2)
    if (! (is.null(xlim) | (is.numeric(xlim) & (length(xlim)==2))))
        stop("'xlim' must be NULL or numeric vector of 2 elements")

    ## xlim (NULL or numeric vector of length 2)
    if (! (is.null(ylim) | (is.numeric(ylim) & (length(ylim)==2))))
        stop("'ylim' must be NULL or numeric vector of 2 elements")
    
    ## Argument matching, ... 'handling'
    time.unit <- match.arg(time.unit)
    test <- match.arg(test)
    dots <- list(...)
    
    ## Restore graph defaults at the end
    if (plot.n.at.risk) {
        old.par <- par(no.readonly=TRUE)
        on.exit(par(old.par))
    }
    
    ## Time divisor for days for the plot axis
    time.divisor <- (time.unit %in% "days" * 1) + 
        (time.unit %in% "weeks" * 7) + 
            (time.unit %in% "months" * 30.43) + 
                (time.unit %in% "years" * 365.25)
    
    ## 30.43 comes from mean(
    ## c(c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,28,31,30,31,30,31,31,30,31,30,31),
    ##   c(31,29,31,30,31,30,31,31,30,31,30,31)))

    
    ## Default xlab and ylab if NULL is provided
    if (is.null(xlab)) {
        ## upcase of time.unit
        xlab <- paste(toupper(substring(time.unit,1,1)),
                      substring(time.unit,2),
                      sep="")
    }
    if (is.null(ylab)) {
        ylab <- "Probability"
    }

    ## Default time.by if NULL is provided
    if (is.null(time.by)) {
        time.by <- if(time.unit=="days") {
            30
        } else if (time.unit=="weeks") {
            4
        } else if (time.unit=="months") {
            12
        } else if (time.unit=="years") {
            1
        }
    }
        
    ## Check if it's a stratified km and set up a few datas
    if (is.null(strata)) {
        univariate <- TRUE
        stratified <- FALSE
        n.strata <- 1
        strata.labels <- c("All")
        my.formula <- survival::Surv(time, status) ~ 1
    } else {
        univariate <- FALSE
        stratified <- TRUE
        stopifnot(all( ! is.na(strata)) )
        n.strata <- nlevels(as.factor(strata))
        strata.labels <- levels(as.factor(strata))
        my.formula <- survival::Surv(time, status) ~ strata
    }

    ## Default conf.int if NULL is provided
    if (is.null(conf.int)) {
        if (univariate) {
            conf.int <- TRUE
        } else {
            conf.int <- FALSE
        }
    }
    

    ## Se si è chiesto un HR, ma gli strati sono piu di 2
    ## tramutarlo in log rank e dare un warning
    if ((n.strata !=2) & (test=="hr")) {
        warning("HR can be plotted only with 2 groups. Changing to Log-rank tests")
        test <- "logr"
    }
    

    ## -------------------------------------
    ## Estimates (km, curve comparison, cox)
    ## -------------------------------------

    ## Kaplan-Meyer survival estimate
    fit <- survival::survfit(my.formula)
    sfit <- summary(fit)
    
    if(stratified) {
        ## Log-rank test
        logr <- survival::survdiff(my.formula)
        logr$df <- n.strata-1
        logr$p <- pchisq( q=logr$chisq, df=logr$df, lower.tail=FALSE )
        logr.string <- sprintf("Log-rank Test=%.2f, df=%d, p %s",	
                               logr$chisq, 
                               logr$df, 
                               pretty_pval(logr$p) )
        ## Cox Model (and summary
        cox <- survival::coxph(my.formula)
        scox <- summary(cox)
        hr.string  <- sprintf("HR=%.3f (95%% CI, %.3f-%.3f)",
                              coefficients(scox)[2],
                              scox$conf.int[3],
                              scox$conf.int[4])
        both.string <- paste(logr.string, hr.string, sep=" - ")
	
        ## Choose which stat to print in the graph
        test.string <- switch(test,
                              logr = logr.string,
                              hr = hr.string,
                              both= both.string) 
    }

    ## ------------
    ## Plot section
    ## ------------

    ## Color set-up: if given, otherwise set it black
    if ( "col" %in% names(dots)) {
        strata.col <- dots$col
    } else {
        strata.col <- rep("black", n.strata)
    }

    ## Se si desidera inserire la tabella dei number at risk
    ## occorre impostare il margine inferiore, prevedendo un tot
    ## di righe opportune (determinate dal numero degli strati) 
    if (plot.n.at.risk) par("oma"=c(n.strata+1,0,0,0))

    ## xlim definition
    if (is.null(xlim)) {
        xlim.inf <-  -(max(fit$time)/15)
        xlim.sup <- max(fit$time) + (- xlim.inf/3)
    } else {
        xlim.inf <- xlim[1]
        xlim.sup <- xlim[2]
    }
        
    ## Axis'n grid: 
    ## y defaults are ok 
    ## x has to be based on time.by, if specified
    ## if not specified make 4 step
    if (is.null(time.by)) {
        times <- seq(0, max(fit$time), length=4)
    } else {
        times <- seq(0, max(fit$time), by=time.by*time.divisor)
    }

    
    ## Main plotting section
    plot(NA,NA, 
         xlim=c(xlim.inf, xlim.sup), 
         ylim=ylim,
         axes=F,
         ylab=ylab,
         xlab=xlab,
         main=main
         )
    axis(2)		
    axis(1, at=times, labels=times/time.divisor)
    add_grid(at.y=axTicks(2), at.x=times)
    box()
    lines(fit, conf.int=conf.int, ...)

    ## Add legend
    if (!is.null(gr.legend)) {
        eval(parse(text=gr.legend))
    }

    ## Add stat string to title
    if (stratified & (test %in% c("logr","hr","both") )) {
        mtext(test.string, line=0.2, family="sans", font=3)
    }

    ## Add number at risk
    if (plot.n.at.risk) {

        ## Print header
        mtext("At risk", side=1, line=4, adj=1, at=xlim.inf,font=2)

        ## Utilizzo axis per plottare gli a rischio negli strati
        ## (la linea utilizzabile in presenza di titolo di asse
        ## delle x è dalla 4 in poi: la 4 è per il titolo, dalla 5
        ## in poi per i dati
        
        my.time <- summary(fit, times = times, extend=TRUE)$time
        n.risk <- summary(fit, times = times, extend=TRUE)$n.risk
        if (univariate) {
            strata <- rep(strata.labels, length(my.time))
        } else {
            strata <- summary(fit, times = times, extend =TRUE)$strata
        }
        
        risk.data <- data.frame(strata = strata,
                                time = my.time,
                                n.risk = n.risk)
        if (!univariate) {
            levels(risk.data$strata) <- sub("(^strata=)(.*$)",
                                            "\\2", 
                                            levels(risk.data$strata))
        }
        
        ## Lo split pone i dati nell'ordine della lista
        ## nell'ordine dei dati, quindi per coerenza con l'ordine
        ## dei colori è necessario riordinare la lista
        
        spl.risk.data <- split( risk.data, risk.data$strata)
        spl.risk.data <- spl.risk.data[ strata.labels ]

        for( label in names(spl.risk.data) ) {
            prog <- which( names(spl.risk.data) %in% label ) 
            group.line.lab <- 4 + prog
            group.line.num <- group.line.lab - 1
            group.col <- strata.col[prog]
            ## plot label del gruppo
            mtext(	label, side=1, line=group.line.lab, 
                  at=xlim.inf, adj=1, col=group.col) 
            ## plot dati per ogni time.by
            axis(1, at=times, labels=spl.risk.data[[label]]$n.risk,
                 line=group.line.num ,tick=FALSE, col.axis=group.col) 
        }

    }

    ## Return Stats wheter or not plot has been done
    if (univariate) {
        return(list("km"=fit))
    } else {
        return(list("km"=fit,"logrank"=logr, "cox"=cox, "scox"=scox))
    }
    
}
