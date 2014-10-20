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
  first <- db[, -ncol(db)]
  second <- db[, -1]

  ## matrix results
  res <- Reduce(operator, list(first, second ))
  colnames(res) <- paste(names(first), names(second), sep = ".vs.")
  if(!is.null(row.id))
    rownames(res) <- row.id

  ## A report with all test that doesn't respect the rule, but selecting
  ## only useful records for print (those with at least 1 query/flag)
  report <- apply(res, 1, function(x) names(x)[x %in% FALSE])
  select <- unlist(lapply(report, function(x) length(x)>0))

  list("results" = res, "report" = report[select])
} 
