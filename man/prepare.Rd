\name{prepare}
\alias{prepare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Prepares a multivariate data set for use in nls / evrunge }
\description{ Utility : \cr
 Modifies a multivariate \code{data.frame} for fitting a nonlinear model defined by a system of first order Ordinary Differential Equations. In the original \code{data.frame}, the data must be arrange in \strong{nfunct + 1} columns, \strong{nfunct} being the dimension of the ODE system. One of the column must be the observation times (or in general the observations of the independent variable). The others colums are the observations of the functions.
 \code{prepare} returns the same data set with a single column for time (repeated \strong{nfunct} times), and a single column for the concatenated observations. A third column is created as a \code{factor} indexing the original column (1 for the first, 2 for the second, etc)
}
\usage{
prepare(x = data.frame, ntime = 1, cols = c(2:(length(x[1, ]))))
}
\arguments{
  \item{x}{  a data.frame : one column for time and one for each function }
  \item{ntime}{ numeric : the number of the column coding for time (independent variable) }
  \item{cols}{ numeric vector : the numbers of the columns corresponding to each dependent variable }
}
\details{
  Prepares the data set as an entry for \code{nls} which is basically univariate. The formula in \code{nls} must involve \code{\link{evrunge}}. As \code{nls} considers the observations as univariate, the criterion used in the least-square process is that of the \strong{trace} of the variance-covariance matrix of the residuals. 
}
\value{
  A data.frame with columns :
   \item{t}{ time repeated as many times as necessary}
   \item{y}{ concatenated observed trajectories }
   \item{traj}{ factor : number of the trajectory }
}
\author{ Jean-Sebastien Pierre 
\email{Jean-sebastien.pierre@univ-rennes1.fr}}
\seealso{ \code{\link{frunge}}, \code{\link{dfdt}},\code{\link{multirunge}},\code{\link{evrunge}},\code{\link{sys}},\code{\link{nls}} }
\examples{
data(syslin.don)
syslin<-prepare(syslin.don)
summary(syslin)
}
\keyword{ nonlinear }% at least one, from doc/KEYWORDS
