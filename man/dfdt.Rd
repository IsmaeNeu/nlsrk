\name{dfdt}
\alias{dfdt}
\title{        Right Side of a first order ODE
}
\description{
	Gives the value of the right side of a first order Ordinary Differential Equation
	Used in frunge (Numeric resolution using Runge-Kutta 4 metthod) 
}
\usage{
	dfdt(t, y, param)
}
\arguments{
  \item{t}{ 	time, independent variable }
  \item{y}{ 	values of the unknown function at t }
  \item{param}{ 	numeric vector : the set of parameters defining the function }
}
\details{
	the unique expression composing the body of dfdt is intended as the right side of the first order
	differential equation :\cr

	\deqn{ \frac{dy}{dt} = dfdt(t,y,param)}{%
	dy/dt = dfdt(t,y,param)}

	The function body has to be written by the user.\cr
  	param must take the form c(param1,param2,param3).
	if the initial value (y0) is used as a parameter (for model fitting for instance), it \strong{must be the last}
	in the list and \strong{not used} in dfdt.
	
}
\value{
	a single float numeric value : the derivative of the unknown function at time t.
}
\references{ Any textbook in mathematics }
\author{ Jean-Sebastien Pierre\cr 
	\email{Jean-sebastien.pierre@univ-rennes1.fr}\cr
}
\note{ When used as argument for frunge, the initial condition \strong{y0} may be used as a parameter subject to fitting by \code{nls}, for instance. In this condition, \strong{y0} must appear as the \strong{last} parameter in param and \strong{must not} be used nor modified in \code{dfdt} 
}
\seealso{ \code{\link{frunge}}, \code{\link{nls}} }
\examples{
## Solves and draws the logistic function using default dfdt as provided in the package	

	frunge(t=10, param=c(r=0.1,k=100), y0=3, Dfdt = dfdt, dt = 0.01, graph = TRUE)

}
\keyword{ nonlinear }% at least one, from doc/KEYWORDS
