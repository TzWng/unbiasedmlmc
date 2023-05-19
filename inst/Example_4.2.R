library(unbiasedmlmc)
## inverse temperature
betas = c(0.20,seq(0.15,0.19,0.01))
nchains = length(betas)
# size of the grid
size <- 12



### MLMC_MCMC
# parameter of Geometric Distribution
p <- 0.7
# nrep samples required
nrep <- 205
# An integer at which to start computing the unbiased estimator
k <- 4000
# A time horizon: the chains are sampled until the maximum between m and the meeting time
m <- 8000
# g function
g1 = function(x){g = x[1]/x[2];return(g)}
n_calls = 2^(rgeom(nrep,p)+1)
Z = rep(0, 2)
H <- list()
### Z(0.15)/Z(0.20)
Z = matrix(rep(0,(nchains-1)*nrep), ncol=1, nrow=nrep)
for (i in 1:nrep){
  #H[[i]] = sampling_range_ising(betas, proba_swapmove, size, k, m, n_calls[i])
  temp1 = sampling_ising_ncon(betas[1], k, m, n_calls[i], size)
  temp2 = sampling_ising_ncon(betas[2], k, m, n_calls[i], size)
  H[[i]] = cbind(temp1,temp2)
  Z[i,1] = try(sample_unbiasedmlmc(g1, H[[i]], p), silent = TRUE)
  print(Z[i,])
}




### standard MCMC estimator
# size of the grid
size <- 12
## function for generate mcmc estimator with same cost as unbiased mcmc_mlmc
mcmc_sample <- function(beta){
  p <- 0.7
  n_calls = 2^(rgeom(nrep,p)+1)
  m = 8000
  total = m*n_calls[i]
  burnin = 0
  chain = 0
  ss_ <- c(-4,-2,0,2,4)
  proba_ <-  exp(ss_*beta) / (exp(ss_*beta) + exp(-ss_*beta))
  # initialization
  current_states <- ising_rinit(size)
  sumstates = 1/exp(beta*ising_sum(current_states))
  # MCMC loop
  for (iteration in 2:total){
    current_states <- ising_single_kernel(current_states, proba_)
    sumstates = 1/exp(beta*ising_sum(current_states))
    if(iteration > burnin){
      chain <- chain + sumstates
    }
  }
  chain_mean = chain/(total - burnin)
  return(chain_mean)
}

## Example 1/Z(theta) where theta form 0.02 to 0.06
nrep = 50
for(i in 1:nrep){
  Z = rep(0,5)
  for(j in 1:length(Z)){
    beta = seq(0.02,0.06,0.01)
    Z[j] = mcmc_sample(beta[j])
  }
  print(Z)
}
