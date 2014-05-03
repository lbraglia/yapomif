\name{make_path}
\alias{make_path}
\alias{make_path.coxph}
\alias{make_path.survreg}

\title{

  Path-function generator.

}

\description{

This function creates another function that paste the directory set up
  in the call to \code{make_path} to the argument passed in the current
  call, allowing better directory parametrazion, auto-explaining code
  and less typing. See example for usage.

}

\usage{

make_path(path)

}


\arguments{
\item{path}{Base path of interest.}
}

%% \author{

%%   \href{http://stat.ubc.ca/~rollin/stats/S/surv.html}{Original S code}
%%   of \code{\link{make_path.coxph}} by Rollin Brant. R porting and
%%   developing by Luca Braglia.

%%   }

%% \references{
%%   Nieto, F.J., Coresh, J. (1996), Adjusting survival curves for
%%   confounders: a review and a new method, American Journal of
%%   Epidemiology, 143:10, 1059-1068.
%% }

%% \seealso{
%%   \code{\link{survfit.coxph}}
%%   }

\value{
  A function that paste the directory set up in the call to
  \code{make_path} to the argument passed in the current call.
}

\examples{
res.dir <- make_path("results")
tmp.dir <- make_path("/tmp")

## Eg for use with write.csv() or pdf()
res.dir("my_results.csv")
tmp.dir("graph.pdf")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ path }

