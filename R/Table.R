#' Cross tabulation and table creation
#' 
#' 
#' This is a wrapper around which adds margins and
#' \code{useNA="if"} by default. 
#' 
#' 
#' @param ... Arguments to be passed to table.
#' @return The function return same results of table with NA (if present) and
#' margins.
#' @export
Table <- function(...) addmargins(base::table(useNA="if", ...))
