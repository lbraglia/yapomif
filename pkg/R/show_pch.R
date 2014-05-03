show_pch <-  function(extras = c("*",".","0","+","#"),
                      cex = 3, ## good for both .Device=="postscript" and "x11"
                      col = "red3", bg = "gold", cextext = 1.2 
                      ) {
        
    nex <- length(extras)
    np  <- 26 + nex
    ipch <- 0:(np-1)
    k <- floor(sqrt(np))
    dd <- c(-1,1)/2
    rx <- dd + range(ix <- ipch %/% k)
    ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
    pch <- as.list(ipch) ## list with integers & strings
    if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
    plot(rx, ry, type = "n", axes  =  FALSE, 
         xlab = "", ylab = "", 
         main = "Pch symbols", 
         sub=sprintf("points (...,  pch = *, cex = '%s', col='%s', bg= '%s')",
             cex, col, bg) ) 
    abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
    for(i in 1:np) {
        pc <- pch[[i]]
        ## 'col' symbols with a 'bg'-colored interior (where available) :
        points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
        if(cextext > 0)
            text(ix[i] - 0.3, iy[i], pc, col = "black", cex = cextext)
    }
    
    ## ShowPch()
    ## ShowPch(c("o","O","0"), cex = 2.5)
    ## ShowPch(NULL, cex = 4, cextext = 0)

    }
