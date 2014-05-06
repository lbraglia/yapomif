\name{getExcelSheet}
\alias{getExcelSheet}
\title{Retrieve an MS Excel (R) sheet from an xls or xlsx file.}
\usage{
getExcelSheet(file=NULL, sheet=NULL, ...)
}
\arguments{
  \item{file}{path to the xls/xlsx file}

  \item{sheet}{sheet name}

  \item{...}{other arguments passed to sqlFetch)}
}
\value{
The function return a data.frame with data from the
specified sheet.
}
\description{
This function is a wrapper around RODBC function to get a
sheet from an xls or xlsx file.
}
\keyword{Excel}
\keyword{get}
\keyword{sheet}

