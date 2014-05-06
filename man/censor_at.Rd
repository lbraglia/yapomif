\name{censor_at}
\alias{censor_at}
\title{Censore time to event end point at a certain time.}
\usage{
censor_at(time, status, censor.time)
}
\arguments{
  \item{time}{Progression Indicator}

  \item{status}{Progression Date}

  \item{censor.time}{Death indicator}
}
\value{
A data frame to be cbinded.
}
\description{
Censore time to event end point at a certain time.
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
\keyword{censor}
\keyword{censoring}
\keyword{cut}
\keyword{death}
\keyword{disease-free}
\keyword{end-point}
\keyword{event}
\keyword{oncology}
\keyword{overall}
\keyword{progression}
\keyword{progression-free}
\keyword{time}
\keyword{to}

