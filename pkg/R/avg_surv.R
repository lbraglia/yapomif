avg_surv <- function(cfit, var.name, var.values,
                             data, weights, pct=0:99/100) {
    UseMethod("avg_surv")
}

avg_surv.coxph <- function(cfit, var.name, var.values, data,
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
        fits <- survfit(cfit, newdata = mframe, se.fit = F)
        curves[[as.character(value)]] <- (fits$surv %*% weights)/sum(
            weights)
    }
    curve.mat <- matrix(unlist(curves), ncol = length(curves), dimnames = 
                        list(NULL, names(curves)))
    fits$surv <- curve.mat
    fits
}


avg_surv.survreg <- function(cfit, var.name, var.values,
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


## OLD STUFF

## avg_surv <- function(cfit,  ...) {
##     UseMethod("avg_surv")
## }

## avg_surv.coxph <- function(cfit, var.name, var.values, data, weights){
##     if(missing(data)) {
##         if(!is.null(cfit$model))
##             mframe <- cfit$model
##         else mframe <- model.frame(cfit, sys.parent())
##     }
##     else mframe <- model.frame(cfit, data)
##     var.num <- match(var.name, names(mframe))
##     data.patterns <- apply(data.matrix(mframe[,  - c(1, var.num)]), 1, 
##                            paste, collapse = ",")
##     data.patterns <- factor(data.patterns, levels=unique(data.patterns))
##     mframe <- mframe[!duplicated(data.patterns),  ]
##     if(missing(weights))
##         weights <- table(data.patterns)
##     else weights <- tapply(weights, data.patterns, sum)
##     curves <- vector(length = length(var.values), mode = "list")
##     names(curves) <- var.values
##     for(value in var.values) {
##         mframe[, var.name] <- value
##         fits <- survfit(cfit, newdata = mframe, se.fit = F)
##         curves[[as.character(value)]] <- (fits$surv %*% weights)/sum(
##             weights)
##     }
##     curve.mat <- matrix(unlist(curves), ncol = length(curves), dimnames = 
##                         list(NULL, names(curves)))
##     fits$surv <- curve.mat
##     fits
## }


## avg_surv.survreg <- function(cfit, var.name, var.values,
##                              data, weights, pct=0:99/100)
## {
##     if (missing(data)) {
##         if (!is.null(cfit$model))
##             mframe <- cfit$model
##         else mframe <- model.frame(cfit, sys.parent())
##     }
##     else mframe <- model.frame(cfit, data)

##     var.num <- match(var.name, names(mframe))
##     data.patterns <- apply(data.matrix(mframe[, -c(1, var.num)]),
##                            1, paste, collapse = ",")
##     data.patterns <- factor(data.patterns, levels = unique(data.patterns))

##     mframe <- mframe[!duplicated(data.patterns), ]
    
##     if (missing(weights)){
##         weights <- table(data.patterns)
##     } else {
##         weights <- tapply(weights, data.patterns, sum)
##     }

##     curves <- vector(length = length(var.values), mode =   "list")
##     names(curves) <- var.values

##     for (value in var.values) {
##         mframe[, var.name] <- value
##         fits <- t(predict(cfit, newdata =  mframe,
##                         type="quantile",
##                         p = pct,
##                         se.fit = F))
##         curves[[as.character(value)]] <-
##             (fits %*%   weights)/sum(weights)
        
##     }

    
##     curve.mat <- matrix(unlist(curves),
##                         ncol =length(curves),
##                         dimnames = list(NULL,  names(curves)))
##     curve.df <- as.data.frame(curve.mat)
##     names(curve.df) <- paste(var.name, names(curve.df), sep="_")
##     curve.df <- cbind("survival" = 1 - pct, curve.df)
##     curve.df
## }
