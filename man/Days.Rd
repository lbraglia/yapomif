\name{Days}
\alias{Days}
\alias{Days.Date}
\alias{Days.POSIXct}
\alias{Days.POSIXlt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extract numeric days of month from a Date, POSIXct or POSIXlt vector.
}
\description{
  
  Extract numeric days of month from a Date, POSIXct or POSIXlt vector.
  
}

\usage{
Days(x)

\method{Days}{Date}(x)

\method{Days}{POSIXct}(x)

\method{Days}{POSIXlt}(x)
}

\arguments{
\item{x}{Date, POSIXct or POSIXlt vector}
}

\value{
  A numeric vector.
}


\examples{
Sys.Date()
Days(Sys.Date())
Days(as.POSIXct(Sys.Date()))
Days(as.POSIXlt(Sys.Date()))

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ day }
\keyword{ Day }
\keyword{ days }
\keyword{ Days }
\keyword{ date }
\keyword{ Date }

