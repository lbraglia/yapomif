\name{getAccessTable}
\alias{getAccessTable}
\title{Retrieve an MS Access (R) table from an mdb or accdb file.}
\usage{
getAccessTable(file=NULL, table=NULL, user="admin", pw=NULL, ...)
}
\arguments{
  \item{file}{path to the mdb/accdb file}

  \item{table}{table name}

  \item{user}{username (login to the file)}

  \item{pw}{password (login to the file)}

  \item{...}{other arguments passed to sqlFetch)}
}
\value{
The function return a data.frame with data from the
specified table.

%% ~Describe the value returned %% If it is a LIST, use %%
\item{comp1 }{Description of 'comp1'} %% \item{comp2
}{Description of 'comp2'}
}
\description{
This function is a wrapper around RODBC function to get a
table from an mdb or accdb file.
}
\keyword{Access}
\keyword{get}
\keyword{table}

