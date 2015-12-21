#' A quick ROC plot.
#' 
#' 
#' A quick ROC plot. It's a simple wrapper around \code{pROC::roc},
#' which adds Youden's criterion optimal cutoff and conditional
#' positioning for AUC (+CI) estimates.
#' 
#' 
#' @param test Marker
#' @param refstd Reference standard (binary variable)
#' @param direction Charachter. Direction passed to \code{pROC::roc}
#' @param plot.roc Logical. Plot ROC curve?
#' @param main Graph main title
#' @return The function plot the graph and return a list with ROC statistics
#' @export
quickROC <- function(test = NULL,
                     refstd = NULL,
                     direction = c("auto", "<", ">"),
                     plot.roc = TRUE,
                     main = "")
{
    
    direction <- match.arg(direction)
    my.roc <- pROC::roc(response = refstd,
                        predictor = test,
                        direction = direction)
    uc.roc <- unclass(my.roc)
    uc.roc$youden <- with(uc.roc, sensitivities + specificities - 1)
    uc.roc$best.thresh <- uc.roc$thresholds[which.max(uc.roc$youden)] 
    my.ci.auc <- as.numeric(as.character(pROC::ci.auc(my.roc)))
    uc.roc$auc.ci <- my.ci.auc[c(1, 3)]

    if (plot.roc) {
        ## AUC estimates sensible (to AUC itself) positioning
        y_auc_base <- 0.15
        x_auc_base <- 0.30
        y_ci_base  <- 0.1
        x_ci_base  <- 0.30
        if (my.ci.auc[2] > 0.5) {
            y_auc <- y_auc_base
            x_auc <- x_auc_base
            y_ci  <- y_ci_base 
            x_ci  <- x_ci_base 
        } else {
            y_auc <- 1 - y_auc_base
            x_auc <- 1 - x_auc_base
            y_ci  <- 1 - y_ci_base 
            x_ci  <- 1 - x_ci_base 
        }
        plot(my.roc,
             print.thres = TRUE,
             print.thres.pattern = "Thresh: %.2f\nSp: %.2f\nSe: %.2f",
             main = main)
        text(x = x_auc, y = y_auc, "AUC: ", pos = 2)
        text(x = x_ci,  y = y_ci, "95% CI: ", pos = 2)
        text(x = x_auc, y = y_auc, sprintf("%.2f", my.ci.auc[2]), pos = 4)
        text(x = x_ci,  y = y_ci, sprintf("%.2f - %.2f",
                                          my.ci.auc[1],
                                          my.ci.auc[3]),
             pos=4)
    }
    uc.roc
}
