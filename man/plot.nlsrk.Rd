\name{plot.nlsrk}
\alias{plot.nlsrk}

\title{ Plot of multivariate data fitted by the nlsrk package }
\description{
  Plot the experimental points of each trajectory with the lines fitted by nls / nlsrk
}
\usage{ \method{plot}{nlsrk}(x, data, maintitle = "Ordinary Differential Equations system",...)
}
\arguments{
  \item{x}{ A "nls" object obtained with \code{evrunge} } 
  \item{data}{ a data.frame with only three columns named (imperatively) t, y, traj in that order }
  \item{maintitle}{ Title for the graph }
  \item{...}{ optional parameters for plot }
}
\details{syslin
  Minimal and preliminary implementation of a plot method for "nls" objects obtained by Runge-Kutta 4 resolution of a set of ODE. The symbols of the experimental points are chosen autom)tically according to the formula pch=19+data$traj. Then, the first trajectory is drawn with pch=20 (black dot), the next with pch=21 (empty circles), etc... see \code{\link{plot}}(\code{\link{options}}) and \code{\link{par}} for details. Similarily, the lines of the fitted solutions are drawn in a color defined automatically according to the formula col=data$traj. The first trajectory is drawn in black (col=1), the next in red (col=2), etc...
Any object of type "nls" is accepted by the program, but if it does not result of the use of evrunge, inpredictible errors will occur. 
}
\value{
	 none
}
\author{ 
jean-sebastien pierre\cr 
	\email{jean-sebastien.pierre@univ-rennes1.fr}\cr
}
\seealso{ \code{\link{evrunge}} }
\examples{
##
##	example 1 : solving and plotting the system sys provided in the package
##
data(syslin.don)
syslin<-prepare(syslin.don)
attach(syslin)
nls(y~ evrunge(t,param=c(k1,k2),y0=c(1000,0),sys,graph=FALSE),data=syslin,
	start=list(k1=1,k2=1),trace=TRUE)->m1
plot.nlsrk(m1,syslin)
##

}
\keyword{ multivariate }% at least one, from doc/KEYWORDS
