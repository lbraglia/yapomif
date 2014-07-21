#' convert a pdf to emf with pstoedit and fig2emf (in Linux)
#'
#' convert a pdf to emf with pstoedit and fig2emf
#' @param files pdf files path vector
#' @export
pdf2emf <- function(files) {

  pstoedit <- Sys.which("pstoedit")
  fig2dev <- Sys.which("fig2dev")
  
  for(file in files) {
    noext <- gsub("\\.[[:alnum:]]+$", "",file)
    path <- paste(noext, c("pdf","emf"), sep = ".")
    tmp.file <- tempfile()
    pdf2fig <- paste(pstoedit, "-f fig", path[1], tmp.file, sep = " ")
    system(command = pdf2fig)
    fig2emf <- paste(fig2dev, "-L emf", tmp.file, path[2], sep = " ")
    system(command = fig2emf)
  }

}



