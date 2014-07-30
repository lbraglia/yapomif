#'
#' Faster export with openxlsx
#'
#' This is a wrapper around addWorksheet and writeData to allow
#' less typing. It export one object per sheet, the sheet will be
#' created accordingly to the name given
#'
#' @param wb a Workbook object
#' @param sheet name of the sheet to be created
#' @param x object to be exported
#' @param ... further arguments passed to writeData
#' @export xlxp
xlxp <- function(wb, sheet, x, ...) {
  openxlsx::addWorksheet(wb = wb, sheetName = sheet)
  openxlsx::writeData(wb = wb,
                      sheet = sheet,
                      x = x,
                      rowNames = TRUE,  ... )
}
