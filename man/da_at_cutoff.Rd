\name{da_at_cutoff}
\alias{da_at_cutoff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate diagnostic accuracy measures for several cutoffs of a
  quantitative marker.
}
\description{

  Calculate diagnostic accuracy measures for several cutoffs of a
  quantitative marker.
    
}

\usage{

da_at_cutoff(cutoffs=NULL, test=NULL, refstd=NULL, round.dig=4, ...  )

 }

\arguments{
\item{cutoffs}{Cutoffs considered} 
\item{test}{Test} 
\item{refstd}{Reference standard} 
\item{round.dig}{Rounding digits} 
\item{...}{parameters passed to da} 
}

\value{
  A data.frame for diagnostic accuracy studies.
}

%% \examples{
%% db <- dadb(tn=720 , tp=190 , fn=10 , fp=80)
%% with(db, table(test,refstd))
%% }



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ diagnostic }
\keyword{ accuracy }
\keyword{ cutoff }

