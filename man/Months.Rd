\name{Months}
\alias{Months}
\alias{Months.default}
\alias{Months.POSIXct}
\alias{Months.POSIXlt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extract numeric months of the year from a Date, POSIXct or POSIXlt vector.
}
\description{
  
  Extract numeric months of the year from a Date, POSIXct or POSIXlt vector.
  
}

\usage{
Months(x, string = TRUE, abbreviate = FALSE )

\method{Months}{POSIXct}(x, string, abbreviate)

\method{Months}{POSIXlt}(x, string, abbreviate)

}

\arguments{
\item{x}{Date, POSIXct or POSIXlt vector}
\item{string}{Logical, if TRUE months string is returned, otherwise numeric}
\item{abbreviate}{logical. Should the months in string format be abbreviated?}

}

\value{
  A vector with extracted months.
}


\examples{
Sys.Date()
Months(Sys.Date())
Months(Sys.Date(), string = FALSE)
Months(Sys.Date(), string = TRUE, abbreviate = TRUE)
## from base package
Months(Sys.Date())
Months(as.POSIXct(Sys.Date()))
Months(as.POSIXlt(Sys.Date()))

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ month }
\keyword{ Month }
\keyword{ months }
\keyword{ Months }
\keyword{ date }
\keyword{ Date }

