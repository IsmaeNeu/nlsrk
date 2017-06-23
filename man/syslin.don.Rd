\name{syslin.don}
\alias{syslin.don}
\docType{data}
\title{ Intra muscular injection (columns) }
\description{
 Concentration of a drug in the muscle and in the blood at various times after injection
}
\usage{data(syslin.don)}
\format{
  A data frame with 50 observations on the following 3 variables.
  \describe{
    \item{\code{t}}{a numeric vector: time}
    \item{\code{y1}}{a numeric vector : muscle concentration}
    \item{\code{y2}}{a numeric vector : blood concentration }
  }
}
\details{
	must be modified by \code{\link{prepare}} to be fitted by nls / evrunge
}
\source{
	Simulated data (J.S. Pierre)
}
\examples{
## concatenation of muscle and blood concentration by prepare

data(syslin.don)
summary(syslin.don)
syslin<-prepare(syslin.don)
summary(syslin)

}
\keyword{datasets}
