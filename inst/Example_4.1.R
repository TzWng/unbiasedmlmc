library(unbiasedmlmc)
d = 8
target <- function(x){
  y <- 1
  for(i in 1:d){
    y = y*dbeta(x[i],i,1)
  }
  y <- log(y)
  return(y)
}


kernels <- get_mh_kernels_beta(target, diag(x=0.1,nrow=d,ncol=d))
# Markov kernel of the chain
single_kernel <- kernels$single_kernel
# Markov kernel of the coupled chain
coupled_kernel <- kernels$coupled_kernel
# initial distribution, towards the right-most mode of the target
rinit <- function(){
  chain_state <- runif(d,0,1)
  current_pdf <- target(chain_state)
  return(list(chain_state = chain_state, current_pdf = current_pdf))
}


g = function(x){
  g = 1/prod(x[1:1])
  return(g)
}

## parameter of Geometric Distribution
p = 0.7
## 0.6, 0.625, 0.65, 0.675, 0.7, 0.725,
## 0.75, 0.775, 0.8
# An integer at which to start computing the unbiased estimator
k <- 4e4
# A time horizon: the chains are sampled until the maximum between m and the meeting time
m <- 16e4
## nrep samples required
nrep = 180
n_calls = 2^(rgeom(nrep,p)+1)
Z = rep(0, nrep)
H <- list()
for (i in 1:nrep){
  H[[i]] =  get_unbiasedmcmc(single_kernel, coupled_kernel, rinit, 
                             h = function(x) x, k = k, m = m,
                             lag = 1, max_iterations = Inf, 
                             n = n_calls[i])
  Z[i] = try(sample_unbiasedmlmc(g, H[[i]], p), silent = TRUE)
  print(Z[i])
}



p = 0.7
# 0.6, 0.625, 0.65, 0.675, 0.7, 0.725,
# 0.75, 0.775, 0.8
# nrep samples required
nrep = 105
n_calls = 2^(rgeom(nrep,p)+1)
Z = rep(0, nrep)
m = 16e4
for (i in 1:nrep){
  current = rinit()
  total = m*n_calls[i]
  burnin = total*0.1
  #chain = matrix(NA, ncol = d, nrow = total)
  chain = rep(0,d)
  for(imcmc in 1:total){
    current <- single_kernel(current)
    current_state <- current$chain_state
    if(imcmc > burnin){
      chain = chain + current_state
    }
  }
  chain_mean = chain/(total - burnin)
  # chain = chain[(burnin+1):total,]
  # col_mean = apply(chain,2,mean)
  # Z[i] = g(col_mean)
  Z[i] = g(chain_mean)
  print(Z[i])
}


