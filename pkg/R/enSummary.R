enSummary <- function(var) c(summary(var), "Sd"=sd(var, na.rm=TRUE))
