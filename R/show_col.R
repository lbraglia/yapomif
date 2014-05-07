#' 
#' Show R colors for graphics and grid package
#' 
#' 
#' Show R colors for graphics and grid package (simple wrapper around functions
#' from demo(colors).
#' 
#' 
#' @usage show_col(package=c("graphics","grid"), bg="white", cex = 0.75,
#' srt.rot=30)
#' @param package charachter. Which package's color to show
#' @param bg charachter. Background color
#' @param cex numeric. cex
#' @param srt.rot degree inclination
#' @return Nothing. As a side effect the plot of colors.
#' @keywords colors color
#' @examples
#' 
#' show_col()
#' 
#' @export show_col
show_col <- function(package=c("graphics","grid"), 
                      bg="white",
                      cex = 0.75, 
                      srt.rot=30
                      ){
   
    ## require("grid")
    ## require("graphics")

    package <- match.arg(package)
    
    showCols.graphics <- function(bg = NULL, cex = NULL, srt = NULL){
        m <- ceiling(sqrt(n <- length(cl <- colors())))
        length(cl) <- m*m; cm <- matrix(cl, m)
        op <- par(mar=rep(0,4), ann=FALSE, bg = bg); on.exit(par(op))
        plot(1:m,1:m, type="n", axes=FALSE)
        text(col(cm), rev(row(cm)), cm,  col = cl, cex=cex, srt=srt)
    }

    showCols.grid <- function(bg=NULL, cex = NULL, rot = NULL) {
        m <- ceiling(sqrt(n <- length(cl <- colors())))
        length(cl) <- m*m; cm <- matrix(cl, m)
        grid::grid.newpage()
        vp <- grid::viewport(width = .92, height = .92)
        grid::grid.rect(gp=grid::gpar(fill=bg))
        grid::grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
                  vp=vp, gp=grid::gpar(cex = cex, col = cm))
    }

    if (package =="graphics") {
        showCols.graphics(bg = bg, cex = cex, srt = srt.rot)
    } else if (package =="grid") {
        showCols.grid(bg = bg, cex = cex, rot = srt.rot)
    }
    
    ## win.graph()
    ## ShowCols("graphics",bg="gray")
    ## win.graph()
    ## ShowCols("grid",bg="black")

}
