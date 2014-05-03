\name{write.delim2}
\alias{write.delim2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Write a tab separated file with "," as decimal separator.
}
\description{
  
  This function is a wrapper around read.table to write tab separated
  files with "." as decimal separator.
  
}

\usage{

write.delim2(x=NULL, file="", row.names=FALSE, col.names=TRUE,
            quote=TRUE, append=FALSE, na="", dec=",")

}

\arguments{
\item{x}{Matrix or data.frame to be exported}
\item{file}{file parameter of write.table}
\item{row.names}{row names}
\item{col.names}{col names}
\item{quote}{quote}
\item{append}{append}
\item{na}{NA string}
\item{dec}{Decimal separator}
}

% \details{
  

% }

\value{
  Nothing returned??

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ tab }
\keyword{ delimited }
\keyword{ format }
\keyword{ file }
\keyword{ output }

