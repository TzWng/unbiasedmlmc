#'@title Ising model unbiased normalizing constant
#'@description Sample an "time-averaged" unbiased estimator of the expected nature statistics in Ising model
#'@param beta inverse temperatures
#'@param k An integer at which to start computing the unbiased estimator
#'@param m A time horizon: the chains are sampled until the maximum between m and the meeting time
#'@param size grid size
#'@return A list with
#'\itemize{
#'
#'\item mcmcestimator: an MCMC estimator computed on the first chain, from step k to m
#'
#'\item correction: the bias correction term
#'
#'\item uestimator: unbiased estimator, equal to the sum of mcmcestimator and correction
#'
#'\item meetingtime: the meeting time; equal to Inf if while loop was interrupted
#'
#'\item iteration: final iteration; could be equal to m, to meetingtime, or to max_iterations
#'
#'\item finished: state of meeting
#'}
#'@export
get_ising_ncon <- function(beta, k = 0, m = 1, size, max_iterations = Inf){
  # possible values of sum of neighbors
  ss_ <- c(-4,-2,0,2,4)
  # precomputed probability for single-site flips
  proba_ <-  exp(ss_*beta) / (exp(ss_*beta) + exp(-ss_*beta))
  # initialize
  chain_state1 <- ising_rinit(size)
  chain_state2 <- ising_rinit(size)
  sumstate1 <- 1/exp(beta*ising_sum(chain_state1))
  sumstate2 <- 1/exp(beta*ising_sum(chain_state2))
  # mcmcestimator computes the natural statistic for each chain
  mcmcestimator <- sumstate1
  dim <- length(mcmcestimator)
  if (k > 0){
    mcmcestimator <- rep(0, dim)
  }
  # move first chain
  iter <- 1
  chain_state1 <- ising_single_kernel(chain_state1, proba_)
  sumstate1 <- 1/exp(beta*ising_sum(chain_state1))
  #correction term computes the sum of min(1, (t - k + 1) / (m - k + 1)) * (h(X_{t+1}) - h(X_t)) for t=k,...,max(m, tau - 1)
  correction <- rep(0, dim)
  if (k == 0){
    correction <- correction + min(1, (0 - k + 1)/(m - k + 1) )  * (sumstate1 - sumstate2)
  }
  # accumulate mcmc estimator
  if (k <= 1 && m >= 1){
    mcmcestimator <- mcmcestimator + sumstate1
  }
  # iterate
  meet <- FALSE
  finished <- FALSE
  meetingtime <- Inf
  # iter here is 1; at this point we have X_1,Y_0 and we are going to generate successively X_t,Y_{t-1} where iter = t
  while (!finished && iter < max_iterations){
    iter <- iter + 1
    if (meet){
      # only need to use single kernel after meeting
      chain_state1 <- ising_single_kernel(chain_state1, proba_)
      sumstate1 <- 1/exp(beta*ising_sum(chain_state1))
      chain_state2 <- chain_state1
      sumstate2 <- sumstate1
      # accumulate mcmc estimator
      if (k <= iter && iter <= m){
        mcmcestimator <- mcmcestimator + sumstate1
      }
    } else {
      # use coupled kernel
      res_ <- ising_coupled_kernel(chain_state1, chain_state2, proba_)
      chain_state1 <- res_$state1
      chain_state2 <- res_$state2
      sumstate1 <- 1/exp(beta*ising_sum(chain_state1))
      sumstate2 <- 1/exp(beta*ising_sum(chain_state2))
      # check if meeting happens
      equal <- all(chain_state1 == chain_state2)
      if (equal && !meet){
        # recording meeting time tau
        meet <- TRUE
        meetingtime <- iter
      }
      if (k <= iter){
        # accumulate mcmc estimator
        if (iter <= m){
          mcmcestimator <- mcmcestimator + sumstate1
        }
        # accumulate correction term
        correction <- correction + min(1, (iter-1 - k + 1)/(m - k + 1) ) * (sumstate1 - sumstate2)
      }
    }
    # stop after max(m, tau) steps
    if (iter >= max(meetingtime, m)){
      finished <- TRUE
    }
  }
  # compute unbiased estimator
  mcmcestimator <- mcmcestimator / (m - k + 1)
  uestimator <- mcmcestimator + correction
  return(list(mcmcestimator = mcmcestimator, correction = correction, uestimator = uestimator,
              meetingtime = meetingtime, iteration = iter, finished = finished))
}



