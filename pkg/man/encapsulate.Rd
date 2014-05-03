\name{encapsulate}
\alias{encapsulate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Encapsulate text between margins.
}
\description{
  Encapsulate text between margins.
    
}

\usage{
encapsulate(s,start="[", last="]",sep="")
 }

\arguments{
\item{s}{string to be encapsulated} 
\item{start}{left margin} 
\item{last}{right margin} 
\item{sep}{separator between string and margins} 
}

\value{
  An encapsulated string.
}

\examples{
encapsulate(letters)
encapsulate(letters, sep=" ")[1:10]

}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graph }
\keyword{ x11 }
\keyword{ window }

