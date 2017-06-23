\name{evrunge}
\alias{evrunge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Numerical Runge-Kutta Solver (multi point, multivariate) }
\description{
  \strong{evrunge} evaluates the solutions of a system of first order ordinary equations for a given set of values of the independent variable (generally the time).
}
\usage{
evrunge(t, param, y0, sys, dt = 0.01, graph = FALSE, observable = rep(1, length(y0)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{ numerical vector : the values of t at which the ODE system must be evaluated }
  \item{param}{ numerical vector or numerical objects list. Passed as arguments to \code{sys} }
  \item{y0}{ numerical vector : the initial values of the unknown functions to solve }
  \item{sys}{ the function giving the right side of the system. Must be written by user (see \code{?sys})}
  \item{dt}{ integration step for Runge-Kutta algorithm. Default = 0.01 }
  \item{graph}{ optionally : graphic representation of the set of solutions. Default =  FALSE}
   \item{observable}{ A numeric vector coding what trajectories are observable. 1 = observable, 2 = not observable. Default : a vector of 1, nfunct times }
}
\details{
  This function is intended basically to be used in conjunction with \strong{nls} for non linear least square
  fit of a set of ODEs on experimental data. This is the reason why the solutions are concatenated in a single column. For fitting with \code{nls}, the data have to be organised in the same way. Use \code{prepare(nlsrk)} to convert a data frame with observations in separate columns in one where the columns are concatenated.
  In a further version of this function, the solutions will optionnaly be provided as columns of an data.frame.
  The system must be provided by user. See \code{\link{sys}} for an editable example.
  
  t \strong{must be} sorted in ascending order \strong{and} every intervals between two consecutive values of t 
  \strong{must be greater then dt}. Otherwise, the function stops and an error message is displayed. 

  Although the algorithm works with \code{dt} slightly lower than with \code{min(t[i+1] - t[i])} the accuracy of the results are not 
  guaranted. It is recommanded to chose with \code{dt} fairly lower than the minimum interval in with \code{t}
  See the note below concerning the parameter 'observable'
}
\value{
  A vector of size \code{nfunct*length(t)} where nfunct is the number of equations in \code{sys} 
}
\references{ ~put references to the literature/web site here ~ }
\author{ Jean-Sebastien Pierre\cr 
	\email{Jean-sebastien.pierre@univ-rennes1.fr}\cr
}

\note{  
  Calls \code{multirunge} for each point \code{t} \cr
  Uses Runge-Kutta 4 algorithm. The error is of order \code{dt^4}. This algorithm is robust and simple but 
  is not guaranteed to work correctly on any ODE system. In particular, RK4 is a fixed step algorithm. "stiff" 
  systems of equations require generally more sophisticated methods with adaptive steps.
 
  Sometimes, especially when the observations are the result of a sampling procedure, it may be useful to treat the initial conditions as parameters injected in the fitting procedure of nls. Note that in these conditions, y0 appears three times in the call to \code{nls}: in \code{param}, as parameter of \code{evrunge} and as starting values for parameters in \code{start}

  The option \code{observable} allows to fit data sets where the data are obzervable only for a subset of the state variables involved in the equations system. There is \strong{no guarantee that nls converges in this case}. The success of the algorithm requires the model to be \strong{identifiable} with the subset of observations. Identifiability is a difficult mathematical topic that filled a lot of books.  
}
\seealso{  \code{\link{multirunge}}, \code{\link{sys}}, \code{\link{prepare}}, \code{\link{nls}} }
\examples{
##
##	example 1 : solving and plotting the system sys provided in the package
##
data(syslin.don)
syslin<-prepare(syslin.don)
  evrunge(t=c(1:30),param=c(1,1),y0=c(1000,0),sys=sys,graph=TRUE)
##
##	example 2 : fitting by nls on data \code{syslin} fixed and known initial conditions
##
data(syslin)
attach(syslin)
nls(y~ evrunge(t,param=c(k1,k2),y0=c(1000,0),sys,graph=FALSE),data=syslin,start=list(k1=1,k2=1),
	trace=TRUE)->m1
summary(m1)
detach(syslin)
##
##	example 3 : fitting by nls on data syslin "unknown" initial conditions: 
##                  they have to be fitted as parameters
##
data(syslin)
nls(y~ evrunge(t,param=c(k1,k2,y0),y0,sys,graph=FALSE),data=syslin,start=list(k1=1,k2=1,
	y0=c(1000,0)),trace=TRUE)->m2
summary(m2)
plot.nlsrk(m2,syslin)
##
##	example 4 : fitting by nls on data syslin with known initial conditions: 
##                  There are no observations for the trajectory 1 which is not observable. 
##		    sys is unchanged
##
data(syslin)
##     We eliminate the data corresponding to trajectory 1 and fit only on trajectory 2. 
##     Fixed initial conditions
 syslin2<-syslin[syslin$traj==2,]
 attach(syslin2)
 nls(y~ evrunge(t,param=c(k1,k2),y0=c(1000,0),sys,graph=FALSE,observable=c(0,1)),data=syslin2,
     start=list(k1=1,k2=1),trace=TRUE)->m3
 summary(m3)
 plot.nlsrk(m3,syslin2)
 detach(syslin2)

}

\keyword{ nonlinear }
