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
}
\description{
This function is a wrapper around RODBC function to get a
table from an mdb or accdb file.
}
\examples{
\dontrun{
 getAccessTable(file="C:/prova.accdb", table="Tabella1")
}
}
\keyword{Access}
\keyword{get}
\keyword{table}

