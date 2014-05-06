\name{Class}
\alias{Class}
\alias{Class.data.frame}
\title{Extract class smartly from more complex data object rather than atomic
vector (eg data.frame) in a handy way}
\usage{
Class(x)

\method{Classdata.frame}(x)
}
\arguments{
  \item{x}{an R object}
}
\value{
A vector of classes
}
\description{
Extract numeric hours of month from a POSIXct or POSIXlt
vector.
}
\examples{
data(Indometh)
Class(Indometh)
}
\keyword{class}
\keyword{typeof}

