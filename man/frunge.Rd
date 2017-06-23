\name{frunge}
\alias{frunge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Numeric solution of an ODE (univariate)
}
\description{
  frunge provides a sampled numeric solution for an ODE for a given set of values of the independent variable (t). Returns a series of values corresponding to each values of t
}
\usage{
frunge(t, param, y0, Dfdt, dt = 0.01, graph = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{ The independent variable. Must be sorted in increasing order }
  \item{param}{ Numeric vector : under the form of \code{c(param1,param2...)} gives the parameters passed to \code{dfdt} }
  \item{y0}{ The initial condition. May appear as a parameter for nls. See details  }
  \item{Dfdt}{ A function giving the right side of the ODE. Default is \code{dfdt(nlsrk)} }
  \item{dt}{ Time increment for Runge-Kutta algorithm. \strong{Must} be lower than the smaller difference between two consecutive \strong{t} values }
  \item{graph}{ If true, plots the graph of the function over the range of t. (For standalone usage) }
}
\details{
  The vector \strong{t must be} sorted in increasing order. Any badly placed \code{t} will result 
  in an error and program exit.	dt must be lower than \code{min(diff(t))}. If not, an error will occur. The shorter \strong{dt}, the more accurate the solution but the longer the calculation will be.
}
\value{
 A numeric vector of length equal to the length of \strong{t}. \strong{y[1]} is set to \strong{y0}
}
\references{ Numerical recipes  \url{http://www.nr.com}}
\author{   Jean-Sebastien Pierre\cr 
	\email{Jean-sebastien.pierre@univ-rennes1.fr}\cr
}
\seealso{ \code{\link{dfdt}},\code{\link{nls}} }
\examples{
	v<-frunge(t=seq(0,50,0.5), param=c(r=0.1,k=100), y0=3, Dfdt = dfdt, dt = 0.01, graph = TRUE)
        plot(seq(0,50,0.5),v)
## 
##     Example of model fitting with frunge. a : determined initial condition 
##
data(logis)
attach(logis)
nls(y~frunge(time,c(r,k),y0=3,dfdt,graph=FALSE),data=logis,start=list(r=0.05,k=100),trace=TRUE)->m1
plot(time,y)
lines(time,fitted(m1))
summary(m1)
detach(logis)
## 
##     Example of model fitting with frunge. b : unknown initial condition ; 
##     y0 is fitted as a parameter

\dontrun{data(logis)
attach(logis)
nls(y~frunge(time,c(r,k,y0),y0,dfdt,graph=FALSE),data=logis,start=list(r=0.05,k=100,y0=y[1]),
    trace=TRUE)->m2
plot(time,y)
lines(time,fitted(m2))
summary(m1)
detach(logis)}

}
\keyword{ nonlinear }% at least one, from doc/KEYWORDS
