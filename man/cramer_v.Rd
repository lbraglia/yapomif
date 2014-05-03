\name{cramer_v}
\alias{cramer_v}
\alias{cramer_v.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

  Cramer's V calculator.

}
\description{
  
  Cramer's V calculator.
  
}

\usage{
cramer_v(x)

\method{cramer_v}{table}(x)

}

\arguments{
\item{x}{an R object. Currently implemented only for tables.}
}

\value{
  A scalar with Cramer's V
}

%% \references{
%%   \href{}{}
  
%% }

\examples{
a <- rnorm(1000) > 0.2
b <- gl(2,500)
cramer_v(table(a,b))

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Cramer }
\keyword{ V }
\keyword{ Phi }

