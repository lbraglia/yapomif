\name{Hours}
\alias{Hours}
\alias{Hours.POSIXct}
\alias{Hours.POSIXlt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extract numeric hours of month from a POSIXct or POSIXlt vector.
}
\description{
  
  Extract numeric hours of month from a POSIXct or POSIXlt vector.
  
}

\usage{
Hours(x)

\method{Hours}{POSIXct}(x)

\method{Hours}{POSIXlt}(x)
}

\arguments{
\item{x}{POSIXct or POSIXlt vector}
}

\value{
  A numeric vector.
}


\examples{
now <- Sys.time()
Hours(now)
Hours(as.POSIXlt(now))

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hour }
\keyword{ Hour }
\keyword{ hours }
\keyword{ Hours }
\keyword{ date }
\keyword{ Date }

