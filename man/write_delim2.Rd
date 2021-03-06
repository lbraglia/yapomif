% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_delim2.R
\name{write.delim2}
\alias{write.delim2}
\title{Write a tab separated file with "," as decimal separator.}
\usage{
write.delim2(x = NULL, file = "", row.names = FALSE, col.names = TRUE,
  quote = TRUE, append = FALSE, na = "", dec = ",")
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
\value{
Return write.table stuff
}
\description{
This function is a wrapper around read.table to write tab separated files
with "." as decimal separator.
}
