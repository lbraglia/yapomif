\name{quick_roc}
\alias{quick_roc}
\title{A quick roc plot.}
\usage{
quick_roc(test, refstd, direction = c("auto","<",">"), plot.roc=TRUE,
main="")
}
\arguments{
  \item{test}{Marker}

  \item{refstd}{Reference standard (binary variable)}

  \item{direction}{Charachter. Direction passed to
  pROC::roc}

  \item{plot.roc}{Logical. Plot roc curve?}

  \item{main}{Graph main title}
}
\value{
The function plot the graph and return a list with roc
statistics
}
\description{
A quick roc plot. It's a simple wrapper around pROC::roc
}
\keyword{ROC}
\keyword{roc}

