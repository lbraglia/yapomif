\name{xtile}
\alias{xtile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Cut numeric x in quantile
}
\description{
  
  Cut numeric x in quantile
  
}

\usage{
xtile(x,n)
}

\arguments{
\item{x}{numeric vector}
\item{n}{number of quantiles}
}

\value{
  A factor vector.
}


\examples{
tertiles <- xtile(rnorm(1000),3)
table(tertiles)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ xtile }
\keyword{ quantiles }
\keyword{ cut }

