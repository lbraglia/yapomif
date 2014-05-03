\name{Years}
\alias{Years}
\alias{Years.Date}
\alias{Years.POSIXct}
\alias{Years.POSIXlt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extract numeric years from a Date, POSIXct or POSIXlt vector.
}
\description{
  
  Extract numeric years from a Date, POSIXct or POSIXlt vector.
  
}

\usage{
Years(x)

\method{Years}{Date}(x)

\method{Years}{POSIXct}(x)

\method{Years}{POSIXlt}(x)

}

\arguments{
\item{x}{Date, POSIXct or POSIXlt vector}
}

\value{
  A numeric vector.
}


\examples{
Sys.Date()
Years(Sys.Date())
Years(as.POSIXct(Sys.Date()))
Years(as.POSIXlt(Sys.Date()))

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ year }
\keyword{ Year }
\keyword{ years }
\keyword{ Years }
\keyword{ date }
\keyword{ Date }

