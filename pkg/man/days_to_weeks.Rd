\name{days_to_weeks}
\alias{days_to_weeks}
\alias{days_to_weeks.numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Trivial function that converts number of days in number of weeks.
}
\description{
  
  Trivial function that converts number of days in number of weeks.
  
}

\usage{
days_to_weeks(days)

\method{days_to_weeks}{numeric}(days)
}

\arguments{
\item{days}{Numeric vector of days}
}

\value{
  A numeric vector of weeks.
}


\examples{
days <- 1:30
days_to_weeks(days)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ day }
\keyword{ days }
\keyword{ week }
\keyword{ weeks }

