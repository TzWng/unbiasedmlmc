nhpv <- c(7, 6, 10, 10, 1, 1, 10, 4, 35, 0, 10, 8, 4)
y1 <- matrix(nhpv, nrow = 1)
# Npart is put in the parameters
Npart <- c(111, 71, 162, 188, 145, 215, 166, 37, 173,
           143, 229, 696, 93)
J <- 13
# posterior is beta in each study, with parameters
posterior_phi_alpha <- 1 + nhpv
posterior_phi_beta <- 1 + Npart - nhpv

sample_module1 <- function(nsamples){
  theta1s <- matrix(nrow = nsamples, ncol = J)
  for (j in 1:J){
    theta1s[,j] <- rbeta(nsamples, shape1 = posterior_phi_alpha[j], shape2 = posterior_phi_beta[j])
  }
  return(theta1s)
}

###
# For module 2, ncases considered data
ncases <- c(16, 215, 362, 97, 76, 62, 710, 56, 133,28, 62, 413, 194)
y2 <- matrix(ncases, nrow = 1)
# Npop considered parameters
Npop <- c(26983, 250930, 829348, 157775, 150467, 352445, 553066,
          26751, 75815, 150302, 354993, 3683043, 507218)
Npop_normalized <- log(10**(-3) * Npop)
# Find parameters given Y2
hyper2 <- list(theta2_mean_prior = 0, theta2_prior_sd = sqrt(1000))
dprior2 <- function(theta2, hyper2){
  return(sum(dnorm(theta2, mean = hyper2$theta2_mean_prior, sd = hyper2$theta2_prior_sd, log = TRUE)))
}


plummer_module2_loglikelihood_ <- function(theta1,theta2,ncases,Npop_normalized){
  eval = 0
  for(j in 1:13){
    logmu = theta2[1] + theta1[j]*theta2[2]+Npop_normalized[j]
    mu = exp(logmu)
    eval = eval + ncases[j]*logmu - mu
  }
  return(eval)
}

## Unbiased estimator for E(lambda_d)
samplejoa <- function(nsamples, k, m){
  ## (modify nsamples if desired)
  # first sample theta1's and compute posterior mean under first model
  theta1s <- sample_module1(nsamples)
  theta1hat <- colMeans(theta1s)
  ## then try to perform inference on theta2 given theta1hat
  Sigma_proposal <- diag(0.1, dimension, dimension)
  init_mean <- rep(0, dimension)
  init_Sigma <- diag(1, dimension, dimension)
  ### Now we run the final pairs of chains
  c_chains_2 <- list()
  theta2s <- matrix(nrow = nsamples, ncol = 2)
  x <- matrix(nrow = nsamples, ncol = 13)
  for(irep in 1:nsamples){
    theta1 <- theta1s[irep,]
    pb <- get_kernels(theta1, Sigma_proposal, init_mean, init_Sigma)
    c_chains_2[[irep]] = sample_coupled_chains(pb$single_kernel, pb$coupled_kernel, pb$rinit, m = m)
    theta2s[irep, ] = H_bar(c_chains_2[[irep]], h = function(x) x, k = k, m = m)
    x[irep, ] = theta2s[irep, 2] + theta1 * theta2s[irep, 1] + Npop_normalized
  }
  return(x)
}


g <- function(x){
  return(max(x))
}


## Unbiased estimator for E[max_d E(lambda_d)]
k <- 2000
m <- 3000
dimension <- 2
## parameter of Geometric Distribution
p = 0.7
# nrep samples required
nrep = 205
n_calls = 2^(rgeom(nrep,p)+1)
for (i in 1:nrep){
  H = samplejoa(n_calls[i], k, m)
  Z = try(sample_unbiasedmlmc(g, H, p), silent = TRUE)
  print(Z)
}



## Standard mcmc estimator under same m and 10% burn-in
dimension <- 2
for (i in 1:nrep){
  nsamples = n_calls[i]
  theta1s <- sample_module1(nsamples)
  theta1hat <- colMeans(theta1s)
  ## then try to perform inference on theta2 given theta1hat
  Sigma_proposal <- diag(0.1, dimension, dimension)
  init_mean <- rep(0, dimension)
  init_Sigma <- diag(1, dimension, dimension)
  x <- matrix(nrow = nsamples, ncol = 13)
  ## first run standard MCMC
  for (j in 1:nsamples){
    theta1 <- theta1s[j,]
    pb <- get_kernels(theta1, Sigma_proposal, init_mean, init_Sigma)
    niterations <- m
    chain <- matrix(0, nrow = niterations, ncol = dimension)
    state <- pb$rinit()
    for (iteration in 1:niterations){
      state <- pb$single_kernel(state)
      chain[iteration,] <- state$chain_state
    }
    chain_postburn <- chain[(m/10):niterations,]
    chain_mean <- colMeans(chain_postburn)
    x[j, ] = chain_mean[2] + theta1 * chain_mean[1] + Npop_normalized
  }
  Z = apply(x,2,mean)
  print(max(Z))
}







