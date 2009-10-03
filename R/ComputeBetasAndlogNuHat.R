`ComputeBetasAndlogNuHat` <-
function(x,y,betainit,lognuinit,max){
# Uses nlm to solve for the MLE estimates for betas and log(nu)

# x = matrix of size nxp where i=1,...,n and j=1,...,p
# y = col vector of size n, i=1,...,n
# betainit = initial vector of betas, b0_1, ..., b0_p
# lognuinit = initial log(nu) value

#create vector of ones
  if(is.matrix(x)==TRUE) {onevec <- rep(1,length(x[,1]))} else onevec <- rep(1,length(x))

#create real X matrix, namely where 1st col is vector of 1s to incorporate beta0 effect
  newx <- cbind(onevec,x)
  xmat <- as.matrix(newx)

# Create -logL so that we take the minimum of this function (which equals the max of logL)
  minusloglike <- function(par){-sum((y * (xmat %*% par[1:length(betainit)])) - (exp(par[length(betainit)+1]) * log(factorial(y))) - log(computez(xmat,par[1:length(betainit)],exp(par[length(betainit)+1]),max)))}

# Determine the MLEs
  BetalogNuEst <- nlm(minusloglike, c(betainit, lognuinit))

return(BetalogNuEst)
}

