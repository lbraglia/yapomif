\name{Minutes}
\alias{Minutes}
\alias{Minutes.POSIXct}
\alias{Minutes.POSIXlt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extract numeric minutes of month from a POSIXct or POSIXlt vector.
}
\description{
  
  Extract numeric minutes of month from a POSIXct or POSIXlt vector.
  
}

\usage{
Minutes(x)

\method{Minutes}{POSIXct}(x)

\method{Minutes}{POSIXlt}(x)
}

\arguments{
\item{x}{POSIXct or POSIXlt vector}
}

\value{
  A numeric vector.
}


\examples{
now <- Sys.time()
Minutes(now)
Minutes(as.POSIXlt(now))

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hour }
\keyword{ Hour }
\keyword{ minutes }
\keyword{ Minutes }
\keyword{ date }
\keyword{ Date }

