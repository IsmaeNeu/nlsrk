\name{syslin}
\alias{syslin}
\docType{data}
\title{ Pharmacokinetics : intramuscular injection }
\description{
	A survey in time of muscle and blood concentrations of a drug following an intramuscular injection.
}
\usage{data(syslin)}
\format{
  A data frame with 100 observations on the following 3 variables.
  \describe{
    \item{\code{t}}{a numeric vector : time in hours }
    \item{\code{y}}{a numeric vector : blood or muscle concentration }
    \item{\code{traj}}{a factor : number of the compartments. 1=muscle, 2= blood}
  }
}
\source{
 simulated data (J.S. Pierre)
}
\examples{
data(syslin)
## maybe str(syslin) ; plot(syslin) ...
}
\keyword{datasets}
