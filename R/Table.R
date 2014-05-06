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
#' 
#' %% ~Describe the value returned %% If it is a LIST, use %% \item{comp1
#' }{Description of 'comp1'} %% \item{comp2 }{Description of 'comp2'}
#' @keywords table Table cross tabulation
#' @export Table
Table <- function(...) addmargins(base::table(useNA="if", ...))
