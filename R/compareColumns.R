#' Compare columns progressively
#'
#' Compare columns progressively in a dataset using a specified operator,
#' that tells how columns should be ordered (eg by default columns should
#' be increasing)
#' @param db a data.frame with ordered columns
#' @param operator comparison operator
#' @param row.id an optional row id
#' @return a list with raw results and a report
#' @examples
#' 
#' compareColumns(data.frame(a = c(1,2,3), b = c(0,1,4)),
#'                row.id = letters[1:3])
#' 
#' @export
compareColumns <- function(db, operator = "<", row.id = NULL) {

  ## data should be a data.frame with no characters
  stopifnot( (is.data.frame(db)) & (!any(sapply(db, is.character))) )

  ## row.id handling
  row.names(db) <- if(is.null(row.id)) {NULL} else {row.id}

  ## Raw results
  res <- data.frame(rep(NA,nrow(db)))
  for (i in 1:(ncol(db)-1))
    eval(parse(text=sprintf("res[,i] <- db[,i] %s db[,i+1]", operator)))
  names(res) <- paste(names(db)[2:(ncol(db))],
                      names(db)[1:(ncol(db)-1)],
                      sep=".vs.")

  ## A report
  report <- apply(res, 1, function(x) names(x)[x %in% FALSE])
  names(report) <- row.id
  ## select useful record for print (those with at least 1 query/flag
  select <- unlist(lapply(report, function(x) length(x)>0))

  list("results" = res, "report" = report[select])
} 




