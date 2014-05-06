\name{write.delim}
\alias{write.delim}
\alias{write.delim2}
\title{Write a tab separated file with "." as decimal separator.}
\usage{
write.delim(x=NULL, file="", row.names=FALSE, col.names=TRUE, quote=TRUE,
append=FALSE, na="", dec=".")

write.delim2(x=NULL, file="", row.names=FALSE, col.names=TRUE, quote=TRUE,
append=FALSE, na="", dec=",")
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

  \item{x}{Matrix or data.frame to be exported}

  \item{file}{file parameter of write.table}

  \item{row.names}{row names}

  \item{col.names}{col names}

  \item{quote}{quote}

  \item{append}{append}

  \item{na}{NA string}

  \item{dec}{Decimal separator}
}
\value{
Nothing returned??

%% ~Describe the value returned %% If it is a LIST, use %%
\item{comp1 }{Description of 'comp1'} %% \item{comp2
}{Description of 'comp2'}

Nothing returned??

%% ~Describe the value returned %% If it is a LIST, use %%
\item{comp1 }{Description of 'comp1'} %% \item{comp2
}{Description of 'comp2'}
}
\description{
This function is a wrapper around read.table to write tab
separated files with "." as decimal separator.

This function is a wrapper around read.table to write tab
separated files with "." as decimal separator.
}
\keyword{delimited}
\keyword{file}
\keyword{format}
\keyword{output}
\keyword{tab}

