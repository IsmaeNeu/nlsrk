\name{multirunge}
\alias{multirunge}

\title{ Runge-Kutta numerical solver (single point, multivariate)
 }
\description{
   Solves numerically an initial conditions problem for a set of Ordinary Differential Equations (ODE) by Runge-Kutta 4 method. Integrates numerically the equations from \strong{tmin} to \strong{tmax} by steps of \strong{dt}. 
}
\usage{
multirunge(y0, tmin, tmax, dt, param, sys)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y0}{ Numerical vector : initial conditions (as many elements as equations in sys) }
  \item{tmin}{ Minimum value of the independent variable (generally the time) }
  \item{tmax}{ Maximum value of the independent variable }
  \item{dt}{ Time increment (default = 0.01) }
  \item{param}{ Numerical vector providing the parameters for sys }
  \item{sys}{ The set of functions giving the right sides of the ODEs }
}
\details{
 \code{sys} must be provided by the user. Please edit the object \code{sys} (see \code{?sys} and examples)
}
\value{
  
  A numerical vector of \strong{nfunct} elements. Nfunct is the number of unknown functions of \code{sys} determined by the function as \code{length(sys())}
}
\author{ Jean-Sebastien Pierre\cr 
	\email{Jean-sebastien.pierre@univ-rennes1.fr}
}
\note{ Should seldom be used independently. Most often it will be called by \strong{evrunge}
}
\seealso{ \code{\link{evrunge}}, \code{\link{sys}}, \code{\link{nls}} }
\examples{ multirunge(y0=c(1000,0),tmin=0,tmax=30,dt=0.01,param=c(1,1),sys=sys)
}
\keyword{ nonlinear }
