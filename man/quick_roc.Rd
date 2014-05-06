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

%% ~Describe the value returned %% If it is a LIST, use %%
\item{comp1 }{Description of 'comp1'} %% \item{comp2
}{Description of 'comp2'}
}
\description{
A quick roc plot. It's a simple wrapper around pROC::roc
}
\keyword{ROC}
\keyword{roc}

