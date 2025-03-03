# initialization of Gibbs chain for Ising model
#'@export
ising_rinit <- function(size = 32){
  initial_values <- 2 * rbinom(size*size, 1, 0.5) - 1
  state <- matrix(initial_values, nrow = size, ncol = size)
  return(state)
}


# initialization of parallel tempering Gibbs chain for Ising model
#'@export
ising_pt_rinit <- function(nchains, size = 32){
  chain_states <- list()
  for (ichain in 1:nchains){
    chain_states[[ichain]] <- ising_rinit(size)
  }
  return(chain_states)
}




# one step of Gibbs sampler, i.e. one full sweep over all components
# probas should be a vector of length 5, containing proba of drawing +1
# given that the sum of neighboring spins is {-4,-2,0,+2,+4}
#'@export
ising_single_kernel <- function(state, probas){
  size = nrow(state)
  itop = 1; ibottom = 1; jright = 1; jleft = 1
  for( i in 1:size){
    for( j in 1:size){
      s = 0;
      itop = i+1; if(itop == (size+1)){itop = 1}
      ibottom = i-1; if(ibottom == 0){ibottom = size}
      jright = j+1; if(jright == (size+1)){jright = 1}
      jleft = j-1; if(jleft == 0){jleft = size}
      s = s + state[itop, j] + state[ibottom, j] + state[i, jright] + state[i, jleft]
      state[i,j] = 2*ifelse(runif(1)<probas[(s+4)/2+1],1,0) - 1
    }
  }
  return(state)
}




# one step of coupled Gibbs sampler, i.e. one full sweep over all components
# probas should be a vector of length 5, containing proba of drawing +1
# given that the sum of neighboring spins is {-4,-2,0,+2,+4}
#'@export
ising_coupled_kernel <- function(state1, state2, probas){
  size = nrow(state1)
  itop = 1; ibottom = 1; jright = 1; jleft = 1
  for( i in 1:size){
    for( j in 1:size){
      s1 = 0; s2 = 0
      itop = i+1; if(itop == (size+1)){itop = 1}
      ibottom = i-1; if(ibottom == 0){ibottom = size}
      jright = j+1; if(jright == (size+1)){jright = 1}
      jleft = j-1; if(jleft == 0){jleft = size}
      s1 = s1 + state1[itop, j] + state1[ibottom, j] + state1[i, jright] + state1[i, jleft]
      s2 = s2 + state2[itop, j] + state2[ibottom, j] + state2[i, jright] + state2[i, jleft]
      u = runif(1)
      state1[i,j] = 2*ifelse(u<probas[(s1+4)/2+1],1,0) - 1
      state2[i,j] = 2*ifelse(u<probas[(s2+4)/2+1],1,0) - 1
    }
  }
  return(list(state1 = state1, state2 = state2))
}




# one step of parallel tempering Gibbs
# chain_states should be a list containing a number N of chains
# sumstates should be a vector containing N integers corresponding to the sum of spins as computed by ising_sum
# betas should be a vector of N inverse temperatures
# probs should be a 5 x N matrix of probabilities of drawing +1 given that the sum of neighboring spins is {-4,-2,0,+2,+4}
# proba_swapmove is a number in (0,1) representing the probability of performing swap moves
#'@export
ising_pt_single_kernel <- function(chain_states, sumstates, betas, probas, proba_swapmove){
  u_iteration <- runif(1)
  nchains <- length(chain_states)
  nswap_attempts <- 0
  nswap_accepts <- rep(0, nchains-1)
  if (u_iteration < proba_swapmove){
    # swap move
    nswap_attempts <- 1
    for (ichain in 1:(nchains-1)){
      tXi <- sumstates[ichain]
      tXip1 <- sumstates[ichain+1]
      swapaccept_logprob <- (betas[ichain] - betas[ichain+1]) * (tXip1 - tXi)
      swapaccept_u <- runif(1)
      if (log(swapaccept_u) < swapaccept_logprob){
        # do swap
        nswap_accepts[ichain] <- 1
        tmp <- chain_states[[ichain]]
        chain_states[[ichain]] <- chain_states[[ichain+1]]
        chain_states[[ichain+1]] <- tmp
        tmp <- sumstates[ichain]
        sumstates[ichain] <- sumstates[ichain+1]
        sumstates[ichain+1] <- tmp
      }
    }
  } else {
    # Gibbs move
    for (ichain in 1:nchains){
      chain_states[[ichain]] <- ising_single_kernel(chain_states[[ichain]], probas[,ichain])
    }
  }
  sumstates <- unlist(lapply(chain_states, ising_sum))
  sumstates <- 1/exp(betas*sumstates)
  return(list(chain_states = chain_states, sumstates = sumstates, nswap_attempts = nswap_attempts, nswap_accepts = nswap_accepts))
}




ising_sum <- function(state){
  size = nrow(state)
  s = 0
  i1 = 1; j1 = 1
  for(i in 1:size){
    if(i == (size)){
      i1 = 1
    } else { i1 = i+1 }
    for(j in 1:size){
      if (j == size){
        j1 = 1
      } else { j1 = j+1 }
      s = s + state[i,j] * (state[i,j1] + state[i1,j])
    }
  }
  return(s)
}




# one step of coupled parallel tempering Gibbs
# chain_states1,chain_states2 should be lists containing a number N of chains
# sumstates1,sumstates2 should be vectors containing N integers corresponding to the sum of spins as computed by ising_sum_
# betas should be a vector of N inverse temperatures
# probs should be a 5 x N matrix of probabilities of drawing +1 given that the sum of neighboring spins is {-4,-2,0,+2,+4}
# proba_swapmove is a number in (0,1) representing the probability of performing swap moves
#'@export
ising_pt_coupled_kernel <- function(chain_states1, chain_states2, sumstates1, sumstates2, betas, probas, proba_swapmove){
  nchains <- length(chain_states1)
  nswap_attempts <- 0
  nswap_accepts1 <- rep(0, nchains-1)
  nswap_accepts2 <- rep(0, nchains-1)
  u_iteration <- runif(1)
  if (u_iteration < proba_swapmove){
    # swap move
    nswap_attempts <- 1
    for (ichain in 1:(nchains-1)){
      tXi_1 <- sumstates1[ichain]
      tXip1_1 <- sumstates1[ichain+1]
      tXi_2 <- sumstates2[ichain]
      tXip1_2 <- sumstates2[ichain+1]
      deltabeta <- betas[ichain] - betas[ichain+1]
      # swapaccept_logprob <- tXi * (-deltabeta) + tXip1 * deltabeta
      swapaccept_logprob1 <- deltabeta * (tXip1_1 - tXi_1)
      swapaccept_logprob2 <- deltabeta * (tXip1_2 - tXi_2)
      swapaccept_u <- runif(1)
      if (log(swapaccept_u) < swapaccept_logprob1){
        # do swap
        nswap_accepts1[ichain] <- 1
        tmp <- chain_states1[[ichain]]
        chain_states1[[ichain]] <- chain_states1[[ichain+1]]
        chain_states1[[ichain+1]] <- tmp
        tmp <- sumstates1[ichain]
        sumstates1[ichain] <- sumstates1[ichain+1]
        sumstates1[ichain+1] <- tmp
      }
      if (log(swapaccept_u) < swapaccept_logprob2){
        # do swap
        nswap_accepts2[ichain] <- 1
        tmp <- chain_states2[[ichain]]
        chain_states2[[ichain]] <- chain_states2[[ichain+1]]
        chain_states2[[ichain+1]] <- tmp
        tmp <- sumstates2[ichain]
        sumstates2[ichain] <- sumstates2[ichain+1]
        sumstates2[ichain+1] <- tmp
      }
    }
  } else {
    # Gibbs move
    for (ichain in 1:nchains){
      res_ <- ising_coupled_kernel(chain_states1[[ichain]], chain_states2[[ichain]], probas[,ichain])
      chain_states1[[ichain]] <- res_$state1
      chain_states2[[ichain]] <- res_$state2
    }
  }
  sumstates1 <- unlist(lapply(chain_states1, ising_sum))
  sumstates1 <- 1/exp(betas*sumstates1)
  sumstates2 <- unlist(lapply(chain_states2, ising_sum))
  sumstates2 <- 1/exp(betas*sumstates2)
  return(list(chain_states1 = chain_states1, chain_states2 = chain_states2,
              sumstates1 = sumstates1, sumstates2 = sumstates2,
              nswap_attempts = nswap_attempts,
              nswap_accepts1 = nswap_accepts1, nswap_accepts2 = nswap_accepts2))
}




# # The function below draws unbiased estimators of the expected natural statistic
# # using "coupled parallel tempering" for the inverse temperatures provided in the vector "betas"
# # and given "k and m" values
# # totalduration is a number in seconds, indicating a time to stop the calculations
# ising_unbiased_estimator <- function(betas, proba_swapmove, size, k = 0, m = 1, max_iterations = Inf, totalduration = Inf){
#   ptm <- proc.time()
#   nchains  <- length(betas)
#   ss_ <- c(-4,-2,0,2,4)
#   probas_ <-  sapply(betas, function(beta) exp(ss_*beta) / (exp(ss_*beta) + exp(-ss_*beta)))
#   # initialize
#   chain_states1 <- ising_pt_rinit(nchains,size)
#   chain_states2 <- ising_pt_rinit(nchains,size)
#   sumstates1 <- unlist(lapply(chain_states1, ising_sum))
#   sumstates1 <- 1/exp(betas*sumstates1)
#   sumstates2 <- unlist(lapply(chain_states2, ising_sum))
#   sumstates2 <- 1/exp(betas*sumstates2)
#   # mcmcestimator computes the natural statistic for each chain
#   mcmcestimator <- sumstates1
#   if (k > 0){
#     mcmcestimator <- rep(0, nchains)
#   }
#   # move first chain
#   iter <- 1
#   res_single_kernel <- ising_pt_single_kernel(chain_states1, sumstates1, betas, probas_, proba_swapmove)
#   chain_states1 <- res_single_kernel$chain_states
#   sumstates1 <- res_single_kernel$sumstates
#   # correction term computes the sum of min(1, (t - k + 1) / (m - k + 1)) * (h(X_{t+1}) - h(X_t)) for t=k,...,max(m, tau - 1)
#   correction <- rep(0, nchains)
#   if (k == 0){
#     correction <- correction + min(1, (0 - k + 1)/(m - k + 1) )  * (sumstates1 - sumstates2)
#   }
#   # accumulate mcmc estimator
#   if (k <= 1 && m >= 1){
#     mcmcestimator <- mcmcestimator + sumstates1
#   }
#   # Check if time is up already
#   elapsedtime <- as.numeric((proc.time() - ptm)[3])
#   if (elapsedtime > totalduration){
#     # time's up
#     return(list(finished = FALSE, message = "interrupted because time's up"))
#   }
#   # iterate
#   meet <- FALSE
#   finished <- FALSE
#   meetingtime <- Inf
#   # iter here is 1; at this point we have X_1,Y_0 and we are going to generate successively X_t,Y_{t-1} where iter = t
#   while (!finished && iter < max_iterations){
#     elapsedtime <- as.numeric((proc.time() - ptm)[3])
#     if (elapsedtime > totalduration){
#       # time's up
#       return(list(finished = FALSE, message = "interrupted because time's up"))
#     }
#     iter <- iter + 1
#     if (meet){
#       # only need to use single kernel after meeting
#       res_ <- ising_pt_single_kernel(chain_states1, sumstates1, betas, probas_, proba_swapmove)
#       chain_states1 <- res_$chain_states
#       sumstates1 <- res_$sumstates
#       chain_states2 <- chain_states1
#       sumstates2 <- sumstates1
#       # accumulate mcmc estimator
#       if (k <= iter && iter <= m){
#         mcmcestimator <- mcmcestimator + sumstates1
#       }
#
#     } else {
#       # use coupled kernel
#       res_ <- ising_pt_coupled_kernel(chain_states1, chain_states2, sumstates1, sumstates2, betas, probas_, proba_swapmove)
#       chain_states1 <- res_$chain_states1
#       chain_states2 <- res_$chain_states2
#       sumstates1 <- res_$sumstates1
#       sumstates2 <- res_$sumstates2
#       # check if meeting happens
#       allequal <- all(sapply(1:nchains, function(i) all(chain_states1[[i]] == chain_states2[[i]])))
#       if (allequal && !meet){
#         # recording meeting time tau
#         meet <- TRUE
#         meetingtime <- iter
#       }
#       if (k <= iter){
#         # accumulate mcmc estimator
#         if (iter <= m){
#           mcmcestimator <- mcmcestimator + sumstates1
#         }
#         # accumulate correction term
#         correction <- correction + min(1, (iter-1 - k + 1)/(m - k + 1) ) * (sumstates1 - sumstates2)
#       }
#     }
#     # stop after max(m, tau) steps
#     if (iter >= max(meetingtime, m)){
#       finished <- TRUE
#     }
#   }
#   # compute unbiased estimator
#   mcmcestimator <- mcmcestimator / (m - k + 1)
#   uestimator <- mcmcestimator + correction
#   return(list(mcmcestimator = mcmcestimator, correction = correction, uestimator = uestimator,
#               meetingtime = meetingtime, iteration = iter, finished = finished))
# }
#
#
#
# sampling_range_ising <- function(betas, proba_swapmove, size, k = 0, m = 1, nrep){
#   # number of dimension
#   d <- length(betas)
#   uestimators <- matrix(NA, ncol = d, nrow = nrep)
#   for( j in 1:nrep){
#     result <- ising_unbiased_estimator(betas, proba_swapmove,  size, k = k, m = m)
#     uestimators[j,] <- result$uestimator
#     }
#   return(uestimators)
# }
#


