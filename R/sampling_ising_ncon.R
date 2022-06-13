#'@title sampling unbiased estimators of nature statistics of Ising model
#'@description Takes in information of Ising model and gives several
#' unbiased estimators for the expected nature statistics
#'@param beta inverse temperatures
#'@param k An integer at which to start computing the unbiased estimator
#'@param m A time horizon: the chains are sampled until the maximum between m and the meeting time
#'@param nrep number of samples needed
#'@param size grid size
#'@return a n*d matrix, where d is the dimension of X and n is the number of samples
#'@export
sampling_ising_ncon <- function(betas, k = 0, m = 1, nrep, size){
  # number of dimension
  d <- length(betas)
  uestimators <- matrix(NA, ncol = d, nrow = nrep)
  for(i in 1:d){
    for( j in 1:nrep){
      result <- get_ising_ncon(betas[i], k = k, m = m, size)
      uestimators[j,i] <- result$uestimator
    }
  }
  return(uestimators)
}

