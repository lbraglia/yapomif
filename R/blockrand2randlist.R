#' Create a randomization list from a blockrand data.frame output 
#'
#' Create a randomization list from a blockrand data.frame output
#' @param x blockrand output
#' @param file path to file to save in (overwriting the contents). If NULL
#' the list is displayed.
#' @param projectTitle a character used in the page footer
#' @examples
#' \dontrun{
#' library(blockrand)
#' set.seed(     6503529)
#' sample.size <- 140
#' Levels <- c("0","1")
#' block.size <- 10
#' randlist <- blockrand(n = sample.size,
#'                       num.levels = length(Levels),
#'                       levels = Levels,
#'                       block.sizes = rep(block.size/2, 2)
#'                       )
#' blockrand2randlist(randlist,
#'                    projectTitle = "PI - Project",
#'                    file = "randlist.xlsx" )
#' 
#' }
#' @export
blockrand2randlist <- function(x, file = NULL, projectTitle = "") {
  ## Add needed columns
  x[c("Cognome.pz", "Nome.pz",
      "Cognome.dr", "Nome.dr",
      "Ora", "Data", "Sigla", "Note"
      )] <- NA
  ## Remove unneeded
  x <- x[c("id", "Cognome.pz",  "Nome.pz",
           "treatment", "Cognome.dr", "Nome.dr",
           "Ora", "Data", "Sigla", "Note" )]
  ## Rename columns
  names(x) <- c("ID", "Cognome",  "Nome",
                "TRAT", "Cognome", "Nome",
                "Ora", "Data", "Sigla di chi risponde", 
                "Note" )
  ## File's header
  header <- matrix(c("Dati del paziente",
                     rep(NA,3),
                     "Dati di chi chiama",
                     NA,
                     "Dati della chiamata",
                     NA,
                     "Sigla di chi risponde",
                     "Note"
                     ), nrow = 1)
  ## Setup the workbook
  wb <- openxlsx::createWorkbook()
  sheetName <- "randomization_list"
  openxlsx::addWorksheet(wb = wb, sheetName = sheetName, 
                         footer = 
                         c("Page &[Page] of &[Pages]", 
                           projectTitle,
                           "&[Date] &[Time]")
                         )
  ## Page setup
  ColConversionFactor <- 6
  RowConversionFactor <- 5.5^2
  headerColWidths <- c(1.2, 3.1, 3.1, 1.5, 3, 3, 1.8, 2.5, 2.2, 5.2)#cm
  headerRowHeights <- 1 # cm
  otherRowHeights <- 2.3# cm
  margins <- 0.4 # inches == 1 cm
  RowHeights <- rep(c(headerRowHeights, otherRowHeights), c(2, nrow(x)))
  openxlsx::pageSetup(wb = wb, sheet = sheetName, 
                      orientation = "landscape",
                      fitToWidth = TRUE, 
                      left = margins,
                      right = margins,
                      top = margins,
                      bottom = margins  
                      )
  openxlsx::setColWidths(wb = wb, sheet = sheetName,
                         cols = 1:10,
                         widths = headerColWidths * ColConversionFactor)
  openxlsx::setRowHeights(wb = wb, sheet = sheetName, 
                          rows = seq_len(length(RowHeights)),
                          heights = RowHeights * RowConversionFactor)
  ## Style
  rlStyle <- openxlsx::createStyle(fontName = "Arial", 
                                   fontSize = 12,
                                   border = "TopBottomLeftRight",
                                   textDecoration = "bold",
                                   halign = "center",
                                   valign = "center",
                                   wrapText=TRUE)
  openxlsx::addStyle(wb = wb, sheet = sheetName, style = rlStyle, 
                     cols = seq_len(ncol(x)),
                     rows = seq_len(nrow(x) + 2), # +2 per l'header 
                     gridExpand = TRUE
                     )
  ## Merge Cells dell'header
  openxlsx::mergeCells(wb = wb, sheet = sheetName, cols = 1:4, rows = 1)
  openxlsx::mergeCells(wb = wb, sheet = sheetName, cols = 5:6, rows = 1)
  openxlsx::mergeCells(wb = wb, sheet = sheetName, cols = 7:8, rows = 1)
  openxlsx::mergeCells(wb = wb, sheet = sheetName, cols = 9, rows = 1:2)
  openxlsx::mergeCells(wb = wb, sheet = sheetName, cols = 10, rows = 1:2)
  ## Data
  openxlsx::writeData(wb = wb, sheet = sheetName, x = header, colNames = FALSE)
  openxlsx::writeData(wb = wb, sheet = sheetName, x = x, startRow = 2)
  ## Save or display
  if (is.null(file))
    openxlsx::openXL(wb)
  else
    openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE)
}
