\name{univTable}
\alias{univTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Univariate table with absolute, relative, cumulative
  frequencies. 
}
\description{
  
  Univariate table with absolute, relative, cumulative
  frequencies. 
  
}

\usage{
univTable(var=NULL, totals=TRUE, useNA="ifany", round.digits=3, sorting=NULL)
 }

\arguments{
\item{var}{vector}
\item{totals}{print totals?}
\item{useNA}{print NA?}
\item{round.digits}{number of rounding digits}
\item{sorting}{sorting can be "asc" or "desc"}
}

\value{
  A matrix with frequencies.
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univariate }
\keyword{ table }
\keyword{ frequencies }

