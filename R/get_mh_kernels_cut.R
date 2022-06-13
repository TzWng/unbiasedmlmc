#'@rdname get_kernels
#'@title Get kernels for cut distribution
#'@description This function takes a sampled parameter(in the first model), initial status
#' and a covariance matrix for a Normal random walk proposal, and returns a list containing
#' the log-pdf target distribution \code{target} as well as \code{single_kernel}, \code{coupled_kernel} corresponding to marginal
#' and coupled MH kernels.
#'
#'
#'@param theta1 posterior mean under first model
#'@param Sigma_proposal covariance of the Normal random walk proposal
#'@param init_mean initial mean of the second model
#'@param init_Sigma initial covariance of the second model
#'@return A list containing the keys
#'\code{target}, \code{single_kernel}, \code{coupled_kernel}, \code{rinit}
#'@export
get_kernels <- function(theta1, Sigma_proposal, init_mean, init_Sigma){
  target <- function(x) plummer_module2_loglikelihood_(theta1, x, ncases, Npop_normalized) + dprior2(x, hyper2)
  ##
  Sigma_proposal_chol <- chol(Sigma_proposal)
  Sigma_proposal_chol_inv <- solve(Sigma_proposal_chol)

  # Markov kernel of the chain
  single_kernel <- function(state){
    chain_state <- state$chain_state
    current_pdf <- state$current_pdf
    proposal_value <- chain_state + fast_rmvnorm_chol(1, rep(0, dimension), Sigma_proposal_chol)[1,]
    proposal_pdf <- target(proposal_value)
    accept <- (log(runif(1)) < (proposal_pdf - current_pdf))
    if (accept){
      return(list(chain_state = proposal_value, current_pdf = proposal_pdf))
    } else {
      return(list(chain_state = chain_state, current_pdf = current_pdf))
    }
  }

  # Markov kernel of the coupled chain
  coupled_kernel <- function(state1, state2){
    chain_state1 <- state1$chain_state
    chain_state2 <- state2$chain_state
    current_pdf1 <- state1$current_pdf
    current_pdf2 <- state2$current_pdf
    proposal_value <- rmvnorm_reflectionmax(chain_state1, chain_state2, Sigma_proposal_chol, Sigma_proposal_chol_inv)
    proposal1 <- proposal_value$xy[,1]
    proposal2 <- proposal_value$xy[,2]
    identical_proposal <- proposal_value$identical
    proposal_pdf1 <- target(proposal1)
    proposal_pdf2 <- proposal_pdf1
    if (!identical_proposal){
      proposal_pdf2 <- target(proposal2)
    }
    logu <- log(runif(1))
    accept1 <- FALSE
    accept2 <- FALSE
    if (is.finite(proposal_pdf1)){
      accept1 <- (logu < (proposal_pdf1 - current_pdf1))
    }
    if (is.finite(proposal_pdf2)){
      accept2 <- (logu < (proposal_pdf2 - current_pdf2))
    }
    if (accept1){
      chain_state1 <- proposal1
      current_pdf1 <- proposal_pdf1
    }
    if (accept2){
      chain_state2 <- proposal2
      current_pdf2 <- proposal_pdf2
    }
    identical_ <- ((proposal_value$identical) && (accept1) && (accept2))
    return(list(state1 = list(chain_state = chain_state1, current_pdf = current_pdf1),
                state2 = list(chain_state = chain_state2, current_pdf = current_pdf2),
                identical = identical_))
  }
  rinit <- function(){
    chain_state <- fast_rmvnorm_chol(1, mean = init_mean, init_Sigma)[1,]
    current_pdf <- target(chain_state)
    return(list(chain_state = chain_state, current_pdf = current_pdf))
  }
  return(list(target = target, coupled_kernel = coupled_kernel, single_kernel = single_kernel, rinit = rinit))
}






