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
