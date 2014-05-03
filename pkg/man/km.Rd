\name{km}
\alias{km}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plots an 'enhanced' Kaplan-Meier plot, with base graphics package.
}
\description{
  
  This function plots a Kaplan-Meier plot.
  
}

\usage{ km(time=NULL, status=NULL, strata=NULL,
time.unit=c("days","weeks","months","years"), time.by=NULL, main="",
ylab=NULL, xlab=NULL, xlim=NULL, conf.int=NULL,
test=c("logr","hr","both","no"), plot.n.at.risk=TRUE, gr.legend=NULL,
...  ) }

\arguments{
  \item{time}{survival time variable}
  \item{status}{survival indicator variable}
  \item{strata}{Stratifying variable (optional)}
  \item{time.unit}{Time unit of x axis}
  \item{time.by}{Time step x axis (in days)}
  \item{main}{Graph main title}
  \item{ylab}{Y-axis label. }
  \item{xlab}{X-axis label. If NULL a suitable default based on
    time.unit will be provided}
  \item{xlim}{X-axis limit. If NULL a suitable default based on
    time.unit will be provided}
  \item{conf.int}{logical ... Plot confidence intervall? If NULL
  confidence interval are plotted only if strata has two or more levels} 
  \item{test}{tests: 'no'=don't plot tests,
    'logr'=logranktest, 'hr'=hazratio, 'both'=both }
  \item{plot.n.at.risk}{Logical value: plot number at risk?}
  \item{gr.legend}{Graph command to add legend, as string}
  \item{...}{Further lines.survfit params}
    
}

% \details{
  

% }

\value{
  The function plot the graph and return a list with km statistics

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Kaplan }
\keyword{ Meier }
\keyword{ survival }

