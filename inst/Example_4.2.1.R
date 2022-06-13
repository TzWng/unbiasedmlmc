library(unbiasedmlmc)
# inverse temperatures
beta <- 0.3
##(0.23, 0.40)

# size of the grid
size <- 32
# parameter of Geometric Distribution
p <- 0.7
# nrep samples required
nrep <- 1000


# An integer at which to start computing the unbiased estimator
k <- 300#8000
# A time horizon: the chains are sampled until the maximum between m and the meeting time
m <- 1500#10000


## deterministic function
g <- function(x){
  y = 1/x
  return(y)
}

### unbiased MLMC_MCMC
n_calls = 2^(rgeom(nrep,p)+1)
Z = rep(0, nrep)
H <- list()
for (i in 1:nrep){
  H[[i]] = sampling_ising_nstat(beta, k=k, m=3*k, n_calls[i], size)
  Z[i] = try(sample_unbiasedmlmc(g, H[[i]], p), silent = TRUE)
  print(Z[i])
}


### meeting time
for(i in 1:nrep){
  result = get_ising_nstat(beta, k = 30, m = 101, size = size)
  print(result$meetingtime)
}




