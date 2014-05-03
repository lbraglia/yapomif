\name{show_col}
\alias{show_col}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

  Show R colors for graphics and grid package

}
\description{
  
  Show R colors for graphics and grid package (simple wrapper around
  functions from demo(colors).
  
}

\usage{
show_col(package=c("graphics","grid"), bg="white",  cex = 0.75,
          srt.rot=30)
}

\arguments{
  \item{package}{charachter. Which package's color to show}
  \item{bg}{charachter. Background color}
  \item{cex}{numeric. cex}
  \item{srt.rot}{degree inclination}
  
}

\value{
  Nothing. As a side effect the plot of colors.
}

%% \references{
%%   \href{}{}
  
%% }

\examples{
show_col()
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ colors }
\keyword{ color }

