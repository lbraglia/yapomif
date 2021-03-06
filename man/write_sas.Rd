% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_sas.R
\name{write_sas}
\alias{write_sas}
\title{Slightly enhanced writeForeignSAS}
\usage{
write_sas(df, datafile, codefile, dataname = "rdata", validvarname = c("V7",
  "V6"), libpath = NULL)
}
\arguments{
\item{df}{same as writeForeignSAS}

\item{datafile}{same as writeForeignSAS}

\item{codefile}{same as writeForeignSAS}

\item{dataname}{same as writeForeignSAS}

\item{validvarname}{same as writeForeignSAS}

\item{libpath}{same as writeForeignSAS}
}
\description{
This version manages factor labels (make.SAS.formats) differently
}
