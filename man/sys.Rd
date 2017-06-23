\name{sys}
\alias{sys}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Right side of a first order differential equation system (multivariate)}
\description{
  \code{sys} gives the set of derivatives of a first order differential equation system. The functions 
  must be provided by user. Edit and change \strong{sys}. 
}
\usage{
sys()
}
%- maybe also 'usage' for other objects documented here.
\details{
  \code{sys} has no arguments but the componing functions must have three : \strong{t, y param}.
  \code{sys} returns the rights sides of the set of \strong{nfonct} functions.
}
\value{
 A numeric vector of the \strong{nfonct} functions at time \strong{t}
}
\references{ any mathematical textbook }
\author{ Jean-Sebastien Pierre\cr 
	\email{Jean-sebastien.pierre@univ-rennes1.fr}
}
\seealso{ \code{\link{evrunge}}, \code{\link{multirunge}}, \code{\link{nls}} }
\examples{

	evrunge(seq(0,10,0.1),c(1,1),y0=c(1000,0),sys=sys,graph=TRUE)

}
\keyword{ nonlinear }% at least one, from doc/KEYWORDS
