#' 
#' Model-based adjusted survival curves.
#' 
#' 
#' This function calculates adjusted survival curves following
#' the "corrected group prognosis" (average of individual
#' survival curve) method.  Based on a fitted Cox proportional
#' hazards model (via \code{\link[survival]{coxph}}) or a
#' parametric model (via \code{\link[survival]{survreg}}), for each
#' specified value of a given variable, the function returns the
#' mean of the family of fitted survival curves relative to the
#' specified variable value and a set of "reference" values for
#' the remaining covariates.
#' 
#' The function organizes a sequence of calls to
#' \code{\link{survfit.coxph}} which does the real work.  The
#' function returns curves which are the averages of covariate
#' specific survival curves, NOT a fitted survival curve at the
#' mean of the covariate values.
#' 
#' @param cfit An object of class "coxph", typically produced by the
#' application of the coxph function.
#' @param var.name A variable from the model represented in cfit.
#' @param var.values Values of the variable defining strata for which mean
#' survival curves are to be calculated.
#' @param data A dataframe with variables corresponding to those represented in
#' cfit.  The values for the covariates (i.e. model variables setting aside the
#' response and the variable specified in var.name) define the reference
#' population.  If data is not specified, the function looks first to see if
#' cfit has a model component (from selecting model=T in the original
#' application of coxph) to use as the data, and failing that, tries to
#' construct a dataframe using model.frame and the parent frame.
#' @param weights An optional vector of weights of length equal to the number
#' of rows of data, used to weight the mean survival curve over the set of
#' reference covariate values.
#' @param pct percentiles of survival curves predicted for
#' \code{\link[survival]{survreg}} objects only
#' @param ... other parameters (for future use).
#' @return An object of class \code{\link{survfit}} (for
#' \code{avgSurv.coxph}) or a \code{\link{data.frame}}
#' (for \code{avgSurv.survreg}), suitable for plotting (see example).
#' @author
#' 
#' \href{http://stat.ubc.ca/~rollin/stats/S/surv.html}{Original S code} of
#' \code{avg_surv.coxph} by Rollin Brant. R porting and development by
#' Luca Braglia.
#' 
#' @seealso \code{\link[survival]{survfit.coxph}}
#' @references Nieto, F.J., Coresh, J. (1996), Adjusting survival curves for
#' confounders: a review and a new method, American Journal of Epidemiology,
#' 143:10, 1059-1068.
#' @keywords average survival corrected group prognosis
#' @examples
#' 
#' ## Variables for fit should be defined prior to application of coxph
#' ## so that model formula contains only already defined variables, i.e. no
#' ## "transformations" of variables, such as Surv or log.
#' 
#' ## Cox PH model
#' data(lung, package = "survival")
#' lung$os <- survival::Surv(lung$time, lung$status)
#' cfit <- survival::coxph(os ~ age + sex + ph.karno + meal.cal+ wt.loss, data=lung)
#' afits <- avgSurv(cfit, "sex", c(1,2))
#' kmfit <- survival::survfit(os ~ sex, data=lung)
#' plot(kmfit, xscale=365.25, col=1:2, lty=2,
#'      main="Kaplan Meier vs \n'Corrected group prognosis' approach")
#' lines(afits, col=1:2, mark.time=FALSE, xscale=365.25)
#' 
#' ## Parametric model
#' pfit <- survival::survreg(os ~ age + sex + ph.karno + meal.cal+ wt.loss,
#'                 data=lung, dist = "exponential")
#' afits2 <- avgSurv(pfit, "sex", c(1,2))
#' lines(afits2$sex_1/365.25, afits2$survival, lty=3, col=1)
#' lines(afits2$sex_2/365.25, afits2$survival, lty=3, col=2)
#' 
#' legend("topright",
#'        legend=c("KM male", "KM female","CGP male (Cox)",
#'        "CGP female  (Cox)", "CGP male (Exp)",
#'        "CGP female  (Exp)" ), 
#'        lty=c(2,2,1,1,3,3),
#'        col=c(1,2,1,2,1,2))
#' 
#' 
#' 
#' 
#' @export
avgSurv <- function(cfit, var.name, var.values,
                             data, weights, pct=0:99/100) {
    UseMethod("avgSurv")
}

#' @export
avgSurv.coxph <- function(cfit, var.name, var.values, data,
                           weights, pct=0:99/100){
    if(missing(data)) {
        if(!is.null(cfit$model))
            mframe <- cfit$model
        else mframe <- model.frame(cfit, sys.parent())
    }
    else mframe <- model.frame(cfit, data)
    var.num <- match(var.name, names(mframe))
    data.patterns <- apply(data.matrix(mframe[,  - c(1, var.num)]), 1, 
                           paste, collapse = ",")
    data.patterns <- factor(data.patterns, levels=unique(data.patterns))
    mframe <- mframe[!duplicated(data.patterns),  ]
    if(missing(weights))
        weights <- table(data.patterns)
    else weights <- tapply(weights, data.patterns, sum)
    curves <- vector(length = length(var.values), mode = "list")
    names(curves) <- var.values
    for(value in var.values) {
        mframe[, var.name] <- value
        fits <- survival::survfit(cfit, newdata = mframe, se.fit = F)
        curves[[as.character(value)]] <- (fits$surv %*% weights)/sum(
            weights)
    }
    curve.mat <- matrix(unlist(curves), ncol = length(curves), dimnames = 
                        list(NULL, names(curves)))
    fits$surv <- curve.mat
    fits
}

#' @export
avgSurv.survreg <- function(cfit, var.name, var.values,
                             data, weights, pct=0:99/100)
{
    if (missing(data)) {
        if (!is.null(cfit$model))
            mframe <- cfit$model
        else mframe <- model.frame(cfit, sys.parent())
    }
    else mframe <- model.frame(cfit, data)

    var.num <- match(var.name, names(mframe))
    data.patterns <- apply(data.matrix(mframe[, -c(1, var.num)]),
                           1, paste, collapse = ",")
    data.patterns <- factor(data.patterns, levels = unique(data.patterns))

    mframe <- mframe[!duplicated(data.patterns), ]
    
    if (missing(weights)){
        weights <- table(data.patterns)
    } else {
        weights <- tapply(weights, data.patterns, sum)
    }

    curves <- vector(length = length(var.values), mode =   "list")
    names(curves) <- var.values

    for (value in var.values) {
        mframe[, var.name] <- value
        fits <- t(predict(cfit, newdata =  mframe,
                        type="quantile",
                        p = pct,
                        se.fit = F))
        curves[[as.character(value)]] <-
            (fits %*%   weights)/sum(weights)
        
    }

    
    curve.mat <- matrix(unlist(curves),
                        ncol =length(curves),
                        dimnames = list(NULL,  names(curves)))
    curve.df <- as.data.frame(curve.mat)
    names(curve.df) <- paste(var.name, names(curve.df), sep="_")
    curve.df <- cbind("survival" = 1 - pct, curve.df)
    curve.df
}
