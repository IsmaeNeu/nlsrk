\name{nlsrk-package}
\alias{nlsrk-package}
\alias{nlsrk}
\docType{package}
\title{
Runge-Kutta package for nls
}
\description{
Performs univariate or multivariate computation of a
single ODE or of a set of ODE (ordinary differential equations)
}
\details{
  \tabular{ll}{
    Package: \tab nlsrk\cr
    Version: \tab 1.0\cr
    Date: \tab 2006-09-07\cr
    Depends: \tab R (>= 1.8.0)\cr
    Suggests: \tab \cr
    License: \tab GPL version 2 or newer\cr
    URL: \tab none\cr
    Built: \tab R 2.2.1; ; 2006-02-22 19:52:45; windows\cr
    }
}

\section{ Univariate functions (single ODE) }{
\preformatted{dfdt                    Right Side of a first order ODE
frunge                  Numeric solution of an ODE (univariate)
}
}
\section{ Multivariate functions (ODEs system)}{
\preformatted{multirunge              Runge-Kutta numerical solver (single point, multivariate)
evrunge                 Numerical Runge-Kutta Solver (multi point, multivariate)
prepare                 Prepares a multivariate data set for use in nls / evrunge
sys                     Right side of a first order differential equation system (multivariate)
}
}
\section{ Utility }{
\preformatted{nlscontour		Draws a contourplot of the sum of squares function around a couple of parameters
summary.nlsgrid         Details on an nlsgrid
}
}
\section{ data }{
\preformatted{logis                   Logistic growth of bacteria
syslin                  Pharmacokinetics : intramuscular injection
syslin.don              Intra muscular injection (columns)
}
}
\note{
 nlsrk provides to nls(stats) an evaluation of functions defined by first order Ordinary Differential Equations (ODEs). For a single ODE use \code{\link{frunge}} and edit \code{\link{dfdt}}. For a system of ODEs, use the multivariate functions \code{\link{evrunge}} and edit  \code{\link{sys}}. The method used in both cases is the Runge-Kutta 4 method. The result is not guaranteed especially for "stiff" systems of equations.
}
\author{
Jean-Sebastien Pierre <jean-sebastien.pierre@univ-rennes1.fr> 
Maintainer: Jean-Sebastien Pierre <jean-sebastien.pierre@univ-rennes1.fr>}
\references{
 Numerical Recipes \url{http://www.nr.com}
}
\keyword{ package }
\seealso{ \code{\link{nls}}
}
