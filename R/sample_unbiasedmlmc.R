#'@title Unbiased ML_MCMC estimators
#'@description Takes in a density function and a deterministic
#'function and gives k unbiased estimators with expectation equal to g(E(X))
#'@param g A deterministic function
#'@param samples i.i.d samples with expectation equal to E(X)
#'@param p parameter of Geometric distribution 
#'@return one unbiased estimator whose expectation is g(E(X))
#'@export 
sample_unbiasedmlmc <- function(g, samples, p){
  d = ncol(samples)
  ## nrep is the number of samples
  nrep = nrow(samples)
  ## get the odd terms of the sample 
  if(is.infinite(g(samples[1,]))){
    samples[1,] = samples[1,] + rbinom(1,0,0.5) - 1/2
  }
  odd = matrix(samples[seq(1,nrep,2),], nrow=nrep/2, ncol=d)
  ## get the even terms of the sample 
  even = matrix(samples[seq(0,nrep,2),], nrow=nrep/2, ncol=d)
  SH_O = apply(odd,2,mean)
  SH_E = apply(even,2,mean)
  SH = 0.5*(SH_O + SH_E)
  repeat{
    if((0%in%SH) & (! 0%in%SH_O)){
      SH_O = SH_O + rbinom(1,0,0.5) - 1/2
      SH = 0.5*(SH_O + SH_E)}
    if(! 0%in%SH){
      a = SH/100
      if(0%in%SH_O){
        SH_O = SH_O + a*(rbinom(1,0,0.5)-1/2)
        SH = 0.5*(SH_O + SH_E)
      }
      if(0%in%SH_E){
        SH_E = SH_E + a*(rbinom(1,0,0.5)-1/2)
        SH = 0.5*(SH_O + SH_E)
      }
    }
    if(! 0%in%(SH_O*SH_E*SH)){break}
  }
  delta_n = g(SH) - (g(SH_O) + g(SH_E))/2
  Z = delta_n/dgeom(log2(nrep)-1,p) + g(samples[1,])
  return(Z)
}
