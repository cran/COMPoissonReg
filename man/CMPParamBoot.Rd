\name{CMPParamBoot}
\alias{CMPParamBoot}

\title{Parametric COM-Poisson Bootstrap}
\description{
  Generates 1000 sample estimates for beta and nu.
} 
\usage{ CMPParamBoot(x, poissonest, betahat, nuhat) }

\arguments{
  \item{x}{explanatory variable data}
  \item{poissonest}{initial estimates from Poisson regression}
  \item{betahat}{COM-Poisson estimates from full data set}
  \item{nuhat}{COM-Poisson dispersion estimate from full data set}
} 

\details{ CMPParamBoot generates 1000 columns of simulated output
(y) data, and uses those simulations along with the explanatory (x)
data to produce 1000 estimates for beta and nu. } 

\value{
  \item{Ystar}{a matrix of values with 1000 columns}
  \item{CMPresult}{a matrix of beta and nu estimates containing 1000 rows}
} 

\references{A Flexible Regression Model for Count Data, by Sellers & Shmueli, http://ssrn.com/abstract=1127359}
\author{Kimberly Sellers}


\seealso{makeCMPdata, ComputeBetasAndNuHat} 

\examples{

}

\keyword{COM-Poisson Parametric Bootstrap}
