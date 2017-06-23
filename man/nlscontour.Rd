\name{nlscontour}
\alias{nlscontour}
\title{ Contour plot for nonlinear least squares }
\description{
  Takes as central values the results of non-linear least square fitting by the nls(base, stats) function, then draws a contour plot of the residual sum of squares function around these values.
}
\usage{
nlscontour(x, param1 = 1, param2 = 2, range1 = NULL, range2 = NULL, 
	npoints = 100, filled = FALSE,colored=FALSE)
}
\arguments{
  \item{x}{ a \code{nls} model }
  \item{param1}{ The number of the first parameter to plot in the parameters list of model formula}
  \item{param2}{ The number of the second parameter to plot }
  \item{range1}{ The range (min and max) for plotting parameter \code{param1}. Default = +/-4*Standard Error of the parameter }
  \item{range2}{ The range (min and max) for plotting parameter \code{param2}. Default = +/-4*Standard Error of the parameter }
  \item{npoints}{  Number of points of the grid for \code{contour}. Default = 100 }
  \item{filled}{ Defines the style of the contour plot FALSE: lines (default), TRUE: filled contours (shades of gray)}
  \item{colored}{ Defines the style of the contour plot FALSE: lines (default), TRUE: Colored contours (red= lower)}
}
\details{
 \code{npoints} defines the total number of points of the square grid used for drawing the contour plot. Thus, the grid will be of size \code{round(sqrt(npoints))*round(sqrt(npoints))}
}
\value{
  an object of class \code{nlsgrid} with three components:
  \item{$x }{ the values used for the first parameter }
  \item{$y }{ the values used for the second parameter}
  \item{$grid }{ a square matrix: The values of the residual sum of squares for each combination of the parameters} ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ Jean-Sebastien Pierre\cr 
	\email{Jean-sebastien.pierre@univ-rennes1.fr}\cr
}
\seealso{ \code{\link{nls}} \code{\link{logis}} \code{\link{summary.nlsgrid}}} 
\examples{
data(logis)
attach(logis)
m1<-nls(y~k/(1+c*exp(-r*time)),data=logis,start=c(k=100,r=0.1,c=40))
nlscontour(m1)
detach(logis)
}
\keyword{ nonlinear }
