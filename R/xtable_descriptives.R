is_date <- function(x) class(x) == 'Date'

make_caption <- function(name){
    name <- gsub('_', '\\\\_', name)
    paste0('\\texttt{', name, '}')
}

make_tables <- function(sums, vertical, removed, datevars){
    for (i in seq_len(length(sums))){
        name <- names(sums)[i]
        if (name %nin% removed) {
            if (name %nin% datevars){
                tmp <- t(as.matrix(sums[[i]]))
            } else {
                tmp <- t(as.character(sums[[i]]))
            }
            if (name %in% vertical){
                tmp <- t(tmp)
                colnames(tmp) <- 'n'
            }
            tab <- xtable::xtable(tmp, 
                                  caption = make_caption(name), 
                                  table.placement = 'h')
            print(tab, include.rownames = name %in% vertical)
        }
    }
}

#' Pretty print data.frame summary in xtables
#'
#' Pretty print data.frame summary in xtables
#'
#' @param x a data.frame
#'
#' @param vertical character vector of db variable names to be printed
#'     vertically. If NULL (by default) all factor are printed
#'     vertically.
#'
#' @param removed character vector of db variable names to be removed
#'     from printing. If NULL (by default) all character variables removed.
#'
#' @export
xtable_descriptives <- function(x, 
                                vertical = NULL,
                                removed = NULL)
{
    
    if (is.null(vertical))
        vertical <- names(which(unlist(lapply(x, is.factor)))) 
    if (is.null(removed))
        removed <- c(names(which(unlist(lapply(x, is.character)))))
    datevars <- names(which(unlist(lapply(x, is_date))))
    
    allv <- lapply(x, summary)
    make_tables(sums = allv,
                vertical = vertical,
                removed = removed,
                datevars = datevars)
}
