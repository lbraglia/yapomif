\name{days_to_months}
\alias{days_to_months}
\alias{days_to_months.numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Trivial function that converts number of days in number of months.
}
\description{
  
  Trivial function that converts number of days in number of months.
  
}

\usage{
days_to_months(days)

\method{days_to_months}{numeric}(days)
}

\arguments{
\item{days}{Numeric vector of days}
}

\value{
  A numeric vector of months.
}


\examples{
days <- 1:30
days_to_months(days)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ day }
\keyword{ days }
\keyword{ month }
\keyword{ months }

