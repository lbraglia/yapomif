\name{tteep}
\alias{tteep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Common time to event end-points calculator.
}
\description{
  
  This function calculates common oncology time to event end-points
  (Overall survival, Progression free survival, time to relapse).
  
}

\usage{
tteep(prog =!is.na(prog.date), # Progression Indicator
      prog.date=NULL, # Progression Date
      death = !is.na(death.date), # Death indicator
      death.date=NULL, # Death date
      start.date=NULL, # Start date
      last.fup=NULL, # Last fup
      OS=TRUE,  # Calculate OS?
      PFS=TRUE, # Calculate PFS?
      TTP=TRUE, # Calculate TTP?
      visual.check=FALSE)

}

\arguments{
\item{prog}{Progression Indicator}
\item{prog.date}{Progression Date}
\item{death}{Death indicator}
\item{death.date}{Death date}
\item{start.date}{Start date}
\item{last.fup}{Last fup}
\item{OS}{Calculate Overall Survival?}
\item{PFS}{Calculate Progression Free Survival?}
\item{TTP}{Calculate Time To Progression?}
\item{visual.check}{View results}
}

% \details{
  

% }

\value{
  A data frame to be cbinded.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ time }
\keyword{ event }
\keyword{ end-point }
\keyword{ outcome }
\keyword{ survival }
\keyword{ overall }
\keyword{ progression-free }
\keyword{ disease-free }
\keyword{ time }
\keyword{ to }
\keyword{ progression }
\keyword{ death }
\keyword{ oncology }

