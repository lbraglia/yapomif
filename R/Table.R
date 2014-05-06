#' Cross tabulation and table creation
#' 
#' 
#' This is a wrapper around which adds margins and useNA="if" by default.
#' 
#' 
#' @usage Table(...)
#' @param ... Arguments to be passed to table.
#' @return The function return same results of table with NA (if present) and
#' margins.
#' @keywords table Table cross tabulation
#' @export Table
Table <- function(...) addmargins(base::table(useNA="if", ...))
