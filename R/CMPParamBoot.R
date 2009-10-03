CMPParamBoot <-
function(x, poissonest, betahat, nuhat){

# Generate 1000 samples, using betahat and nuhat from full dataset
Ystar <- matrix(0,nrow=nrow(x),ncol=1000)
for (i in 1:1000){
   Ystar[,i] <- makeCMPdata(x,betahat,nuhat)
   }

# Take each of the 1000 sample results along with the x matrix, and run CMP regression on it to generate new betas and nu
CMPresult <- matrix(0,nrow=1000,ncol=length(betahat)+1)
for (i in 1:1000){
   CMPresult[i,] <- ComputeBetasAndNuHat(x,Ystar[,i],poissonest,nuinit=1,max=100)$par 
   }

return(list(Ystar=Ystar, COMPresult=CMPresult))
}

