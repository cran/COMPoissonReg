\name{ComputeBetasAndlogNuHat} 
\alias{ComputeBetasAndlogNuHat}

\title{Estimate COM-Poisson parameters: betas and log(nu)}

\description{
 Estimates maximum likelihood estimates for betas and log(nu) using nlm.
}

\usage{ ComputeBetasAndlogNuHat(x, y, betainit, lognuinit, max) }

\arguments{
  \item{x}{matrix of size nxp}
  \item{y}{column vector of length n}
  \item{betainit}{initial vector of betas, b0_1, ..., b0_p}
  \item{lognuinit}{initial value for log(nu)}
  \item{max}{maximum number to use for truncating infinite sums}
}

\details{
ComputeBetasAndlogNuHat finds the maximum likelihood estimates for the COM-Poisson estimators using unconstrained optimization (nlm).  The dispersion parameter is estimated on the log scale. n=number of records and p=number of predictors.
}

\value{
  Maximum likelihood estimates for betas and log(nu) using unconstrained optimization (nlm).
} 

\references{A Flexible Regression Model for Count Data, by Sellers & Shmueli, http://ssrn.com/abstract=1127359}
\author{Kimberly Sellers}


\seealso{ComputeBetasAndNuHat}

\examples{
}


\keyword{maximum likelihood estimation}
\keyword{regression}
\keyword{estimate}
