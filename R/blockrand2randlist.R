#' Create a randomization list from a list of blockrand generated data.frame 
#'
#' Create a randomization list from a list of blockrand generated data.frame
#' 
#' @param x named list of blockrand data.frame (names will be used as
#'          sheet name)  
#' @param f path to file to save in (overwriting the contents). If NULL
#' the list is displayed.
#' @param footer a character vector used in the page footer (recicled)
#' @export
blockrand2randlist <- function(x, f = NULL, footer = "") {

    ## normalize_x in order to handle both single data.frame and list
    ## of data.frames
    normalize_x <- function(x){
        if (is.data.frame(x)){
            x <- list(x)
            names(x) <- '1'
        } else if (is.list(x)) {
            if (is.null(names(x)))
                names(x) <- as.character(seq_len(length(x)))
        } else
            stop('x must be a data.frame or a list of data frames')
        
        return(x)
    }

    x <- normalize_x(x)
    sheet_names <- names(x)

    if (!(is.character(footer) && length(footer) == 1L))
        stop('footer must be a character of length 1')
    
    if (!((is.character(f) && length(f) == 1L) || is.null(f)))
        stop('f must be a character of length 1 or NULL')

    ## modify each data frame to a proper output format
    x <- lapply(x, function(rl){
        ## Add needed columns
        rl[c("Cognome.pz", "Nome.pz",
             "Cognome.dr", "Nome.dr",
             "Ora", "Data", "Sigla", "Note"
             )] <- NA
        ## Remove unneeded stuff
        rl <- rl[c("id", "Cognome.pz",  "Nome.pz",
                   "treatment", "Cognome.dr", "Nome.dr",
                   "Ora", "Data", "Sigla", "Note" )]
        ## Rename columns
        names(rl) <- c("ID", "Cognome",  "Nome",
                       "TRAT", "Cognome", "Nome",
                       "Ora", "Data", "Sigla di chi risponde", 
                       "Note" )
        ## change to alfanumeric id
        rl$ID <- to_00_char(rl$ID, floor(log10(max(rl$ID))) + 1)

        return(rl)
    })

    ## Sheets' header
    header_inv <- header_tc <- matrix(c("Dati del paziente",
                                        rep(NA,3),
                                        "Dati di chi chiama",
                                        NA,
                                        "Dati della chiamata",
                                        NA,
                                        "Sigla di chi risponde",
                                        "Note"
                                        ), nrow = 1)
    header_inv[1, c(5,9)] <- c("Dati di chi risponde",
                               "Sigla di chi chiama")

    
    ## Setup the workbook
    wb <- openxlsx::createWorkbook()
    lapply(sheet_names, function(s)
        openxlsx::addWorksheet(wb = wb,
                               sheetName = s, 
                               footer = c("Page &[Page] of &[Pages]", 
                                          sprintf("%s[Strato: %s]", footer, s),
                                          "&[Date] &[Time]") )
        )
  
    ## Page setup variables
    ColConversionFactor <- 6
    RowConversionFactor <- 5.5^2
    headerColWidths <- c(1.2, 3.1, 3.1, 1.5, 3, 3, 1.8, 2.5, 2.2, 5.2)#cm
    headerRowHeights <- 1 # cm
    otherRowHeights <- 2.3# cm
    margins <- 0.4 # inches == 1 cm
    RowHeights <- rep(c(headerRowHeights, otherRowHeights), c(2, nrow(x[[1]])))
    rlStyle <- openxlsx::createStyle(fontName = "Arial", 
                                     fontSize = 12,
                                     border = "TopBottomLeftRight",
                                     textDecoration = "bold",
                                     halign = "center",
                                     valign = "center",
                                     wrapText=TRUE)

    ## Do for each sheet/dataset
    lapply(sheet_names, function(s){

        openxlsx::pageSetup(wb = wb,
                            sheet = s,
                            scale = 83, # per far stare tutto
                            orientation = "landscape",
                            fitToWidth = TRUE, 
                            left = margins,
                            right = margins,
                            top = margins,
                            bottom = margins * 1.5)

        openxlsx::setColWidths(wb = wb,
                               sheet = s,
                               cols = 1:10,
                               widths = headerColWidths * ColConversionFactor)

        openxlsx::setRowHeights(wb = wb,
                                sheet = s, 
                                rows = seq_len(length(RowHeights)),
                                heights = RowHeights * RowConversionFactor)

        openxlsx::addStyle(wb = wb,
                           sheet = s,
                           style = rlStyle, 
                           cols = seq_len(ncol(x[[s]])),
                           rows = seq_len(nrow(x[[s]]) + 2), # +2 per l'header 
                           gridExpand = TRUE)
    
        ## Merge Cells dell'header
        openxlsx::mergeCells(wb = wb, sheet = s, cols = 1:4, rows = 1)
        openxlsx::mergeCells(wb = wb, sheet = s, cols = 5:6, rows = 1)
        openxlsx::mergeCells(wb = wb, sheet = s, cols = 7:8, rows = 1)
        openxlsx::mergeCells(wb = wb, sheet = s, cols =   9, rows = 1:2)
        openxlsx::mergeCells(wb = wb, sheet = s, cols =  10, rows = 1:2)
    })


    ## -----------------------------
    ## Trial Center List - full list
    ## -----------------------------
    wb_tc <- wb
    lapply(sheet_names, function(s) {
        ## header
        openxlsx::writeData(wb = wb,
                            sheet = s,
                            x = header_tc,
                            colNames = FALSE)
        ## data
        openxlsx::writeData(wb = wb_tc,
                            sheet = s,
                            x = x[[s]],
                            startRow = 2)
    })
    if (is.null(f)) {
        openxlsx::openXL(wb_tc)
    } else {
        tc_file <- paste0(f, '_TRIAL_CENTER.xlsx')
        openxlsx::saveWorkbook(wb = wb_tc, file = tc_file, overwrite = TRUE)
    }

    ## -----------------------------
    ## Investigators - blanked list
    ## -----------------------------
    if (!is.null(f)) {
        wb_inv <- wb
        lapply(sheet_names, function(s) {
            ## header
            openxlsx::writeData(wb = wb,
                                sheet = s,
                                x = header_inv,
                                colNames = FALSE)
            ## data
            tmp <- x[[s]]
            tmp$TRAT <- NA
            openxlsx::writeData(wb = wb_inv,
                                sheet = s,
                                x = tmp,
                                startRow = 2)
        })
        
        inv_file <- paste0(f, '_INVESTIGATORS.xlsx')
        openxlsx::saveWorkbook(wb = wb_inv, file = inv_file, overwrite = TRUE)

    }
}
