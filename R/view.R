#'
#' Alternative to View that uses openxlsx's openXL
#'
#' Alternative to View that uses openxlsx's openXL
#'
#' @param db a data.frame to be viewed
#' @export view
view <- function(db)  {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Data")
    openxlsx::writeData(wb, "Data", db)
    openxlsx::openXL(wb)
}
