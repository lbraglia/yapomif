\name{notIn}
\alias{\%nin\%}
\title{ Not in }
\description{
  Negation of the \code{\%in\%} operator.
}
\usage{
  x \%nin\% table
}
\arguments{
  \item{x}{The values to be matched}
  \item{table}{The values to \emph{not} be matched against}
}
\value{
  Logical vector, negation of the \code{\%in\%} operators on the same arguments.
}
\author{ Original author: Romain Francois }
\examples{                        
  1:10 \%nin\% c(1,3,5,9)  
}
\keyword{ utilities }

