\name{Days}
\alias{Days}
\alias{Days.Date}
\alias{Days.POSIXct}
\alias{Days.POSIXlt}
\title{Extract numeric days of month from a Date, POSIXct or POSIXlt vector.}
\usage{
Days(x)

\method{DaysDate}(x)

\method{DaysPOSIXct}(x)

\method{DaysPOSIXlt}(x)
}
\arguments{
  \item{x}{Date, POSIXct or POSIXlt vector}
}
\value{
A numeric vector.
}
\description{
Extract numeric days of month from a Date, POSIXct or
POSIXlt vector.
}
\examples{
Sys.Date()
Days(Sys.Date())
Days(as.POSIXct(Sys.Date()))
Days(as.POSIXlt(Sys.Date()))
}
\keyword{Date}
\keyword{Day}
\keyword{Days}
\keyword{date}
\keyword{day}
\keyword{days}

