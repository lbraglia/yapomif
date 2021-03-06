% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cutoffDA.R
\name{cutoffDA}
\alias{cutoffDA}
\title{Calculate diagnostic accuracy measures for several cutoffs of a quantitative
marker.}
\usage{
cutoffDA(cutoffs = NULL, test = NULL, refstd = NULL, round.dig = 4, ...)
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
\description{
Calculate diagnostic accuracy measures for several cutoffs of a quantitative
marker.
}
