\name{Class}
\alias{Class}
\alias{Class.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extract class smartly from more complex data object rather than atomic
  vector (eg data.frame) in a handy way

}
\description{
  
  Extract numeric hours of month from a POSIXct or POSIXlt vector.
  
}

\usage{
Class(x)

\method{Class}{data.frame}(x)

}

\arguments{
\item{x}{an R object}
}

\value{
  A vector of classes
}


\examples{
data(Indometh)
Class(Indometh)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ class }
\keyword{ typeof }

