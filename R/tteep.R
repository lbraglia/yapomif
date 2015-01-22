#' Common time to event end-points calculator.
#' 
#' This function calculates common oncology time to event end-points (Overall
#' survival, Progression free survival, time to relapse).
#' 
#' @param prog Progression Indicator
#' @param prog.date Progression Date
#' @param death Death indicator
#' @param death.date Death date
#' @param start.date Start date
#' @param last.fup Last fup
#' @param OS Calculate Overall Survival?
#' @param PFS Calculate Progression Free Survival?
#' @param TTP Calculate Time To Progression?
#' @param visual.check View results
#' @return A data frame to be used with \code{\link{cbind}}.
#' @export
tteep <- function(prog =!is.na(prog.date), # Progression Indicator
                  prog.date=NULL, # Progression Date
                  death = !is.na(death.date), # Death indicator
                  death.date=NULL, # Death date
                  start.date=NULL, # Start date
                  last.fup=NULL, # Last fup
                  OS=TRUE,  # Calculate OS?
                  PFS=TRUE, # Calculate PFS?
                  TTP=TRUE, # Calculate TTP?
                  visual.check=FALSE)
{
  ## CHECK
  if(is.null(start.date)|is.null(last.fup))
    stop("\n\tNeeded following vars (at least): 
				\t - start.date, 
				\t - last.fup")
  
  ## controlla che tutte le variabili indicatrici sian
  ## correttamente codificate
  if(!all(c(prog,death) %in% c(0,1))) 
    stop("Not all prog,death %in% 01")
  
  ## controlla che laddove ci sia una variabile 
  ## indicatrice a 1 vi sia la data corrispondente
  
  ## results list (to be converted to dataframe)
  res <- list()
  
  if (OS) { # Overall survival
    dummy <- death
    last.date <- ifelse(dummy,death.date, last.fup) 
    last.date <- as.Date(last.date, origin="1970-01-01")
    time <- as.numeric(last.date - start.date)
    res$os <- data.frame("dummy"=as.numeric(dummy),
                         "last.date"=last.date,
                         "time"=time)
    rm(dummy,last.date,time)
  } 
  
  if (PFS) { # Progression Free Survival
    dummy <- death | prog
    min.prog.death <- apply(cbind(death.date,prog.date), 1,
                            min, na.rm=T)
    min.prog.death <- as.Date(min.prog.death, origin="1970-01-01")
    last.date <- ifelse(dummy, min.prog.death, last.fup)
    last.date <- as.Date(last.date, origin="1970-01-01")
    time <- as.numeric(last.date - start.date)
    res$pfs <- data.frame("dummy"=as.numeric(dummy),
                          "last.date"=last.date,
                          "time"=time)
    rm(dummy,last.date,time)
    
  } 

  if (TTP) { # Time to progression
    dummy <- prog
    min.death.lfup <- apply(cbind(death.date,last.fup), 1,
                            min, na.rm=T)
    min.death.lfup <- as.Date(min.death.lfup, origin="1970-01-01")
    last.date <- ifelse(dummy, prog.date, min.death.lfup)
    last.date <- as.Date(last.date, origin="1970-01-01")
    time <- as.numeric(last.date - start.date)
    res$ttp <- data.frame("dummy"=as.numeric(dummy),
                          "last.date"=last.date,
                          "time"=time)
    rm(dummy,last.date,time)
    
  } 
	
	
  ## Visualizza i risultati a video:
  ## TODO: attualmente fallisce se visual.check=T e uno delle 
  ## componenti è null perchè non viene fornito alla chiamata
  ## ideale sarebbe comporre il dataframe "check" con i soli elementi
  ## necessari (derivanti dalla chiamata es se OS=T,PFS=F,TTP=F)
  
  if (visual.check) {
    check <- cbind(
      "start.date"=start.date,
      "prog"=as.numeric(prog),
      "prog.date"=prog.date,
      "death"=as.numeric(death),
      "death.date"=death.date, 
      "last.fup"=last.fup,
      as.data.frame(res))
    check <- check[ order(check$death,check$prog),]
    view(check)
  }
  
  as.data.frame(res)
}

