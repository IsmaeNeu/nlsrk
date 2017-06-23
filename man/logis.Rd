\name{logis}
\alias{logis}
\docType{data}
\title{ Logistic growth of bacteria }
\description{
 Logistic growth in bacteria : time versus Optical Density in arbitrary units (simulated data)
}
\usage{data(logis.don)}
\format{
  A data frame with 51 observations on the following 2 variables.
  \describe{
    \item{\code{time}}{a numeric vector : time in hours }
    \item{\code{y}}{a numeric vector : Optical Density }
  }
}
\details{
  a dataset suitable to run nls and fit a logistic model, eventually under the form of a differential equation 
}
\source{
  personnal source J.S. Pierre
}
\examples{
data(logis)
## maybe str(logis) ; plot(logis) ...
}
\keyword{datasets}
