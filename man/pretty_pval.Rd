\name{pretty_pval}
\alias{pretty_pval}
\title{Pretty print for p-values}
\usage{
pretty_pval(pvalue, space)
}
\arguments{
  \item{pvalue}{A numeric vector of p-values.}

  \item{space}{Logical specifying whether a space shoud be
  inserted between number and operator (= or <, default to
  FALSE).}
}
\value{
The function return a string with the pretty printed
p-values.
}
\description{
Pretty print for p-values.
}
\examples{
pretty_pval(0.3)

pval1 <- c(3, NA, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05)
pretty_pval(pval1, space = TRUE)
}
\keyword{p}
\keyword{p-value}
\keyword{p-values}
\keyword{pretty}
\keyword{print}
\keyword{value}
\keyword{values}

