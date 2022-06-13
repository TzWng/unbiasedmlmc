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
k <- 800#8000
# A time horizon: the chains are sampled until the maximum between m and the meeting time
m <- 1000#10000
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







### MCMC
# probability of performing swap moves
proba_swapmove <- 0.01
niterations <- 1e5 + 8000
# history
history_sumstates <- matrix(0, ncol = nchains, nrow = niterations)
# precomputed probability for single-site flips
ss_ <- c(-4,-2,0,2,4)
probas_ <-  sapply(betas, function(beta) exp(ss_*beta) / (exp(ss_*beta) + exp(-ss_*beta)))
# initialization
current_states <- ising_pt_rinit(nchains, size)
sumstates <- unlist(lapply(current_states, ising_sum))
sumstates <- 1/exp(betas*sumstates)
history_sumstates[1,] <- sumstates
nswap_attempts <- 0
nswap_accepts <- rep(0, nchains-1)
# MCMC loop
for (iteration in 2:niterations){
  res_ <- ising_pt_single_kernel(current_states, sumstates, betas, probas_, proba_swapmove)
  current_states <- res_$chain_states
  sumstates <- res_$sumstates
  nswap_accepts <- nswap_accepts + res_$nswap_accepts
  nswap_attempts <- nswap_attempts + res_$nswap_attempts
  history_sumstates[iteration,] <- sumstates
}

mcmc_estimates = apply(history_sumstates[8001:niterations,],2,mean)
#mcmc_estimates[1:5]
for(i in 8001:niterations){
  print(history_sumstates[i,1:5])
}
