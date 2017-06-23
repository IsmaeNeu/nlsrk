\name{kmk}
\alias{kmk}
\title{  mck-Mc Kendrick SIR system }
\description{
 a variant of \code{sys} to use in \code{evrunge}
}
\usage{
kmk()
}
%- maybe also 'usage' for other objects documented here.
\value{
  see \code{\link{sys}} Three values for three equations are returned
}

\author{ J.S. Pierre }
\seealso{ \code{\link{sys}}, \code{\link{evrunge}}  }
\examples{

## The function is currently defined as
function () 
{
    f1 <- function(t, y, param) -param[1] * y[1] * y[2]
    f2 <- function(t, y, param) param[1] * y[1] * y[2] - param[2] * y[2]
    f3 <- function(t, y, param) param[2] * y[2]
    c(f1, f2, f3)
  }
}
\keyword{ nonlinear }% at least one, from doc/KEYWORDS
