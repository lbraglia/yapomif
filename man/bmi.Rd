\name{bmi}
\alias{bmi}
\title{Calculate BMI}
\usage{
bmi(weight.kg = NULL, height.m = NULL, bmi = NULL,
    categ = FALSE, labels = FALSE)
}
\arguments{
  \item{weight.kg}{weight in kg}

  \item{height.m}{height in m}

  \item{bmi}{bmi already computed}

  \item{categ}{logical indicating whether to return a
  factor (who classification)}

  \item{labels}{if categ == TRUE, logical indicating
  whether to return factor with labels (who
  classification)}
}
\value{
a vector of bmi
}
\description{
Calculate bmi
}
\examples{
## Simple calculation
bmi(w = 85, h = 1.85)
## Who classes without or with labels
bmi(b = 24, categ = TRUE)
bmi(b = 27, categ = TRUE, labels = TRUE)
}

