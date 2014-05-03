\name{censor_at}
\alias{censor_at}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Censore time to event end point at a certain time.
}
\description{
  
  Censore time to event end point at a certain time.
  
}

\usage{
censor_at(time, status, censor.time)

}

\arguments{
\item{time}{Progression Indicator}
\item{status}{Progression Date}
\item{censor.time}{Death indicator}
}

% \details{
  

% }

\value{
  A data frame to be cbinded.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}

}

\examples{

    time   <- c(100,150)
    status <- c(  0,  1)
    censor.time <- 120
    (original <- data.frame(time, status))
    censor_at(time=time, status=status, censor.time = 160)
    censor_at(time=time, status=status, censor.time = 150)
    censor_at(time=time, status=status, censor.time = 125)
    censor_at(time=time, status=status, censor.time = 75)

    time   <- c(100,150)
    status <- c(  1,  0)
    censor.time <- 120
    (original <- data.frame(time, status))
    censor_at(time=time, status=status, censor.time = 160)
    censor_at(time=time, status=status, censor.time = 150)
    censor_at(time=time, status=status, censor.time = 125)
    censor_at(time=time, status=status, censor.time = 75)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ time }
\keyword{ event }
\keyword{ end-point }
\keyword{ censor }
\keyword{ cut }
\keyword{ censoring }
\keyword{ overall }
\keyword{ progression-free }
\keyword{ disease-free }
\keyword{ time }
\keyword{ to }
\keyword{ progression }
\keyword{ death }
\keyword{ oncology }

