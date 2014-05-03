\name{graph}
\alias{graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Open a vectorial device file in a portable way.
}
\description{

  Portable function which choose what (vectorial) device to open looking
  at the system. 
  
}

\usage{
graph(file, width,  height,  ...)
}

\arguments{
  \item{file}{path without extension}
  \item{width}{width in inches}
  \item{height}{height in inches}
  \item{...}{other arguments passed to \code{\link{pdf}} or
    \code{\link[grDevices]{win.metafile}}} 
}

\value{
  Values returned by pdf or win.metafile?
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ pdf }
\keyword{ win.metafile }
\keyword{ vectorial }
\keyword{ graph }

