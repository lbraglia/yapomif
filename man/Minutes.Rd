\name{Minutes}
\alias{Minutes}
\alias{Minutes.POSIXct}
\alias{Minutes.POSIXlt}
\title{Extract numeric minutes of month from a POSIXct or POSIXlt vector.}
\usage{
Minutes(x)

\method{MinutesPOSIXct}(x)

\method{MinutesPOSIXlt}(x)
}
\arguments{
  \item{x}{POSIXct or POSIXlt vector}
}
\value{
A numeric vector.
}
\description{
Extract numeric minutes of month from a POSIXct or POSIXlt
vector.
}
\examples{
now <- Sys.time()
Minutes(now)
Minutes(as.POSIXlt(now))
}
\keyword{Date}
\keyword{Hour}
\keyword{Minutes}
\keyword{date}
\keyword{hour}
\keyword{minutes}

