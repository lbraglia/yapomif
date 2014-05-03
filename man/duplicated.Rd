\name{duplicated}
\alias{duplicated}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Determine all duplicate elements
}
\description{

  base::duplicated determines which elements of a vector or data frame are
  duplicates of elements with smaller subscripts, and returns a logical
  vector indicating which elements (rows) are duplicates.
  
  This wrapper allows to find all duplicated obs.

}

\usage{

duplicated( x, all.dup = FALSE, ...)

}

\arguments{
  \item{x}{Numeric vector of centimeter.}
  \item{all.dup}{Logical: if TRUE considers duplicated each observation
  present more than one time in the oject, otherwise implement
  base::duplicated algorithm (considers duplicated the second, third and
  so on duplicated observation).}
  \item{...}{Other arguments passed to base::duplicated.}

}

% \details{
  
% }

\value{
  A logical vector with duplicated marked as TRUE.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}

}

%% \examples {

%% all.equal(duplicated(iris), base::duplicated(iris))
%% all.equal(duplicated(iris, all.dup=TRUE), base::duplicated(iris))
%% sum(base::duplicated(iris))
%% sum(duplicated(iris, all.dup=TRUE))


%% }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ duplicated }

