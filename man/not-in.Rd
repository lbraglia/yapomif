\name{\%nin\%}
\alias{\%nin\%}
\title{Not in}
\usage{
x %nin% table
}
\arguments{
  \item{x}{The values to be matched}

  \item{table}{The values to \emph{not} be matched against}
}
\value{
Logical vector, negation of the \code{%in%} operators on
the same arguments.
}
\description{
Negation of the \code{%in%} operator.
}
\examples{
1:10 \%nin\% c(1,3,5,9)
}
\author{
Original author: Romain Francois
}
\keyword{in}
\keyword{not}

