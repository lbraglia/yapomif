\name{dummy}
\alias{dummy}
\title{Generate a data.frame of dummy variables given a vector}
\usage{
dummy(x, prefix="")
}
\arguments{
  \item{x}{a vector}

  \item{prefix}{name prefix of returned data.frame columns}
}
\value{
a data.frame to be cbinded
}
\description{
Generate a data.frame of dummy variables given a vector
}
\examples{
rep(1:3, 4)
dummy(rep(1:3, 4))
}

