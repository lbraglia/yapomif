\name{dir.clean}
\alias{dir.clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

  Remove all files in a directory.

}
\description{
  
  Remove all files in a directory.
  
}

\usage{
dir.clean(directory)

}

\arguments{
\item{directory}{Charachter. A path to directory to be cleaned}
}

\value{
  Logical. Operation successful or no files in the directory.
}

%% \references{
%%   \href{}{}
  
%% }

\examples{
tmp.dir <- tempdir()
file.create(paste(tmp.dir,"foo", sep = "/"))
dir.clean(tmp.dir)
dir(tmp.dir)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Cramer }
\keyword{ V }
\keyword{ Phi }

