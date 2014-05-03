\name{days_to_years}
\alias{days_to_years}
\alias{days_to_years.numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Trivial function that converts number of days in number of years.
}
\description{
  
  Trivial function that converts number of days in number of years.
  
}

\usage{
days_to_years(days)

\method{days_to_years}{numeric}(days)
}

\arguments{
\item{days}{Numeric vector of days}
}

\value{
  A numeric vector of years.
}


\examples{
days <- seq(1, 365.25*3, by = 60)
days_to_years(days)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ day }
\keyword{ days }
\keyword{ year }
\keyword{ years }

