\name{Years}
\alias{Years}
\alias{Years.Date}
\alias{Years.POSIXct}
\alias{Years.POSIXlt}
\title{Extract numeric years from a Date, POSIXct or POSIXlt vector.}
\usage{
Years(x)

\method{YearsDate}(x)

\method{YearsPOSIXct}(x)

\method{YearsPOSIXlt}(x)
}
\arguments{
  \item{x}{Date, POSIXct or POSIXlt vector}
}
\value{
A numeric vector.
}
\description{
Extract numeric years from a Date, POSIXct or POSIXlt
vector.
}
\examples{
Sys.Date()
Years(Sys.Date())
Years(as.POSIXct(Sys.Date()))
Years(as.POSIXlt(Sys.Date()))
}
\keyword{Date}
\keyword{Year}
\keyword{Years}
\keyword{date}
\keyword{year}
\keyword{years}

