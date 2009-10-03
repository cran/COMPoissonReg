\name{COM-PoissonReg-package}

\alias{COM-PoissonReg-package}
\alias{COM-PoissonReg}

\docType{package}

\title{estimate parameters for COM-Poisson regression}

\description{ This package offers the ability to compute the COM-Poisson parameter estimates (via the ComputeBetasAndNu or ComputeBetasAndLogNu functions) and associated standard errors (via the CMPStdErrors functions).  This package also provides a hypothesis test for determining statistically significant data dispersion, and other model diagnostics.}

\details{
\tabular{ll}{ Package: \tab COM-PoissonReg\cr Type: \tab Package\cr
Version: \tab 1.0\cr Date: \tab 2009-10-03\cr License: \tab GPL-2 |
GPL-3\cr LazyLoad: \tab yes\cr } 

This package offers the ability to compute the COM-Poisson parameter estimates (via the ComputeBetasAndNu or ComputeBetasAndLogNu functions) and associated standard errors (via the CMPStdErrors functions).

Further, the user can perform a hypothesis test (using the function, LRT) to determine the statistically significant need for using COM-Poisson regression to model the data.  The test addresses the matter of statistically significant dispersion.

The main order of functions is as follows:

1.  Compute Poisson estimates (using standard glm)
2.  Use Poisson estimates as starting values to determine COM-Poisson estimates (using ComputeBetasAndNu)
3.  Compute associated standard errors (using CMPStdErrors function)

From here, there are lots of ways to proceed, so order doesn't matter:
 - Perform a hypothesis test to assess for statistically significant dispersion (using LRT)
 - Compute leverage (using leverage) and deviance (using CMPDev)

}

\author{
Kimberly Sellers
Maintainer: Thomas Lotze <thomas.lotze@thomaslotze.com> 
}

\references{A Flexible Regression Model for Count Data, by Sellers & Shmueli, http://ssrn.com/abstract=1127359}

\examples{
# load freight data
  data(freight)                                             
# airfreight breakage (breakage of ampules filled with some biological substance are shipped in cartons).
# X (col 2) = number of times carton was transferred from one aircraft to another
# Y (col 1) = number of ampules found broken upon arrival. 

# Compute Poisson estimates
  freight.est <- summary(glm(freight[,1] ~ freight[,2],family=poisson,na.action=na.exclude))$coeff[,1] # beta estimates
  print("The Poisson estimates for the beta vector are")
  print(freight.est)

# Compute COM-Poisson estimates (under constant dispersion model)
  freight.result <- ComputeBetasAndNuHat(freight[,2],freight[,1],freight.est,nuinit=1,max=100) 
  if(freight.result$convergence == 1) {print("*** NOTE: Constant CMP estimates could not be determined.  Optimization scheme did not converge.  Type 'freight.result' for details.")}

# Compute associated standard errors for constant COM-Poisson estimates
  freight.StdErrors <- CMPStdErrors(freight[,2],freight.result$par[1:length(freight.result$par)-1],freight.result$par[length(freight.result$par)],max=100)
  print("The associated standard errors for the betas in the constant dispersion case are")
  print(freight.StdErrors[1:length(freight.StdErrors)-1])

# Perform likelihood ratio test for dispersion parameter
  # Test for dispersion equal or not equal to 1 (ie performing Poisson vs COM-Poisson regression)
  freight.test <- LRT(freight[,2],freight[,1],freight.est,freight.result$par[1:length(freight.result$par)-1],freight.result$par[length(freight.result$par)],max=100) 
  print("The likelihood ratio test statistic and associated p-value (testing Poisson vs CMP regression) are")
  print(freight.test)

# Compute constant COM-Poisson leverage
  freight.lev <- leverage(freight[,2],freight[,1],freight.result$par[1:length(freight.result$par)-1],freight.result$par[length(freight.result$par)],max=100)

# Compute constant COM-Poisson deviances
  freight.CMPDev <- CMPDeviance(freight[,2],freight[,1],freight.result$par[1:length(freight.result$par)-1],freight.result$par[length(freight.result$par)],freight.lev,max=100)
  print("The approximate constant dispersion standardized CMP Deviance is")
  print(freight.CMPDev)

# Compute fitted values
  freight.constantCMPfitsandresids <- constantCMPfitsandresids(freight.result$par[1:length(freight.result$par)-1],freight.result$par[length(freight.result$par)],freight[,2],freight[,1])
  print("The CMP fit and residuals are")
  print(freight.constantCMPfitsandresids)

# Compute MSE
  freight.constantCMP.MSE <- mean(freight.constantCMPfitsandresids$resid^2)
  print("The MSE for the constant CMP regression is")
  print(freight.constantCMP.MSE)
}
