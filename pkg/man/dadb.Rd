\name{dadb}
\alias{dadb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Generate a data.frame of diagnostic accuracy study.
}
\description{
  Generate a data.frame of diagnostic accuracy study given true
  negative, true positive, false negative, false positive.
    
}

\usage{
dadb(tn,tp,fn,fp)
 }

\arguments{
\item{tn}{True negative} 
\item{tp}{True positive} 
\item{fn}{False negative} 
\item{fp}{False positive} 
}

\value{
  A data.frame for diagnostic accuracy studies.
}

\examples{
db <- dadb(tn=720 , tp=190 , fn=10 , fp=80)
with(db, table(test,refstd))
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ diagnostic }
\keyword{ accuracy }

