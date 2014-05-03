\name{date_mdy}
\alias{date_mdy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a \code{\link{Date}} from month, day and year.
}
\description{
  
  Create a \code{\link{Date}} from month, day and year.
  
}

\usage{
date_mdy( month, day, year) 
}

\arguments{
\item{month}{Numeric vector of months}
\item{day}{Numeric vector of days}
\item{year}{Numeric vector of years}
}

% \details{
  

% }

\value{
  A vector of \code{\link{Date}}.
}


\examples{
db <- data.frame(month=10:12, day=15:17, year=1982:1984)
date_mdy(month = db$month, day= db$day, year=db$year)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ month }
\keyword{ day }
\keyword{ year }
\keyword{ date }
\keyword{ Date }
\keyword{ months }
\keyword{ days }
\keyword{ years }

