#'@rdname fast_rmvnorm
#'@title fast_rmvnorm
#'@description Generate multivariate Normal draws. The function does not check
#' the arguments, use at your own risk.
#'@param n An integer >= 1 specifying the desired number of draws
#'@param mean A vector of size d specifying the mean vector of the multivariate Normal
#'@param covariance A matrix of size d x d specifying the covariance matrix of the multivariate Normal
#'@return A matrix of size n x d containing n d-dimensional multivariate Normal draws (one per row)
#'@examples
#'  fast_rmvnorm(2, rep(0, 5), diag(1, 5, 5))
#'@export
fast_rmvnorm = function(n, mean, covariance){
  ncols = ncol(covariance)
  Y = matrix(NA, nrow=n, ncol = ncols)
  for(j in 1:ncols){
    Y[,j] = rnorm(n)
  }
  Y = Y %*% covariance
  for(j in 1:ncols){
    for(i in 1:n){
      Y[i,j] = Y[i,j] + mean[j]
    }
  }
  return(Y)
}



#'@rdname fast_rmvnorm_chol
#'@title fast_rmvnorm_chol
#'@description Generate multivariate Normal draws. The function does not check
#' the arguments, use at your own risk.
#'@param n An integer >= 1 specifying the desired number of draws
#'@param mean A vector of size d specifying the mean vector of the multivariate Normal
#'@param chol A matrix of size d x d specifying the upper triangular Cholesky factor
#'of the covariance matrix of the multivariate Normal target,
#'for instance obtained using the \code{\link[base]{chol}}
#'function of R.
#'@return A matrix of size n x d containing n d-dimensional multivariate Normal draws (one per row)
#'@examples
#' Sigma <- diag(1, 5, 5)
#' Sigma[1,2] <- Sigma[2,1] <- 0.3
#' fast_rmvnorm_chol(2, rep(0, 5), chol(Sigma))
#'@export
fast_rmvnorm_chol = function(nparticles, mean, chol){
  ncols = ncol(chol)
  Y = matrix(NA, nrow=nparticles, ncol = ncols)
  for(j in 1:ncols){
    Y[,j] = rnorm(nparticles)
  }
  Y = Y %*% chol
  for(j in 1:ncols){
    for(i in 1:nparticles){
      Y[i,j] = Y[i,j] + mean[j]
    }
  }
  return(Y)
}


