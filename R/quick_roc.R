#' A quick roc plot.
#' 
#' 
#' A quick roc plot. It's a simple wrapper around pROC::roc
#' 
#' 
#' @usage
#' 
#' quick_roc(test, refstd, direction = c("auto","<",">"), plot.roc=TRUE,
#' main="")
#' @param test Marker
#' @param refstd Reference standard (binary variable)
#' @param direction Charachter. Direction passed to pROC::roc
#' @param plot.roc Logical. Plot roc curve?
#' @param main Graph main title
#' @return The function plot the graph and return a list with roc statistics
#' @keywords ROC roc
#' @export quick_roc
quick_roc <- function(test, refstd,
                      direction =
                      c("auto","<",">"),
                      plot.roc=TRUE,
                      main="")
{
    direction <- match.arg(direction)
    my.roc <- pROC::roc(response = refstd,
                        predictor = test,
                        direction = direction)
    
    uc.roc <- unclass(my.roc)
    uc.roc$youden <- with(uc.roc, sensitivities +
                          specificities - 1)
    uc.roc$best.thresh <- uc.roc$thresholds[uc.roc$youden ==
                                            max(uc.roc$youden)] 
    my.ci.auc <- as.numeric(as.character(ci.auc(my.roc)))
    uc.roc$auc.ci <- my.ci.auc[c(1,3) ]

    if (plot.roc) {
        plot(my.roc, print.thres=T,
             print.thres.pattern="Thresh: %.0f\nSp: %.3f\nSe: %.3f",
             main = main   )
        text(x=0.4, y=0.30, "AUC: ",   pos=2)
        text(x=0.4, y=0.25, "95% CI: ", pos=2)
        text(x=0.4, y=0.30, sprintf("%.3f",my.ci.auc[2]), pos=4)
        text(x=0.4, y=0.25, sprintf("%.3f - %.3f",my.ci.auc[1],
                        my.ci.auc[3]), pos=4)
    }
    uc.roc
}
