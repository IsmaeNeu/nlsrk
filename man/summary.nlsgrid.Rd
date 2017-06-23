\name{summary.nlsgrid}
\alias{summary.nlsgrid}
\title{ Summary method for objects of class nlsgrid(nlsrk) }
\description{
  Prints the characteristics of an nlsgrid
}
\usage{
\method{summary}{nlsgrid}(object, ...)
}
\arguments{
  \item{object}{ An object of class nlsgrid }
  \item{\dots}{ Any parameters for summary }
}
\value{
NULL
}
\references{ none }
\author{  Jean-Sebastien Pierre\cr 
	\email{Jean-sebastien.pierre@univ-rennes1.fr}\cr
}
\seealso{  \code{\link{nlsrk}}, \code{\link{nlsrk-package}}, \code{\link{nls}}, \code{\link{nlscontour}}}
\examples{
data(logis)
m1<-nls(y~k/(1+c*exp(-r*time)),data=logis,start=list(k=100,r=0.1,c=45))
gr12<-nlscontour(m1)
summary(gr12)
}
\keyword{ nonlinear }
