#'@rdname rmvnorm_reflectionmax
#'@title Reflection-Maximal coupling of two multivariate Normal distributions
#'@description Sample from reflection-maximal coupling of two multivariate Normal distributions,
#'specified through their means, with the same covariance matrix, specified
#'through its Cholesky factor and inverse of Cholesky factor.
#'
#'The idea is that a multivariate Normal is drawn around the first mean (mu1),
#'and then reflected with respect to a hyperplane orthogonal to the direction between mu1 and mu2.
#'
#'For univariate Normal distribution, see \code{\link{rnorm_reflectionmax}}.
#'
#'@param mu1 First mean
#'@param mu2 First mean
#'@param Cholesky Cholesky factor, e.g. obtained with \code{\link[base]{chol}}
#'@param Cholesky_inverse Inverse of Cholesky factor, e.g. obtained with \code{solve(chol(Sigma))}
#'@return A list containing 'xy', a matrix with 2 columns (one for each draw),
#' and a boolean indicator 'identical' indicating whether the two draws
#' are identical.
#'@examples
#' p <- 3
#' mu1 <- rep(0, p)
#' mu2 <- rep(1, p)
#' Sigma <- diag(0.4, p, p)
#' Sigma[1,2] <- Sigma[2,1] <- 0.2
#' Sigma_chol <- chol(Sigma)
#' Sigma_chol_inv <- solve(Sigma_chol)
#' rmvnorm_reflectionmax(mu1, mu2, Sigma_chol, Sigma_chol_inv)
#'@export
rmvnorm_reflectionmax <- function(mu1, mu2, Cholesky, Cholesky_inverse){
  d = length(mu1)
  xy = matrix(NA, ncol = 2, nrow = d)
  identical = FALSE
  scaled_diff = (mu2-mu1) %*% Cholesky_inverse
  xi = rnorm(d,0,1)
  eta = vector()
  z = - scaled_diff
  normz = sqrt(sum(z^2))
  if (normz < 1e-15){
    identical = TRUE
    xi = mu1 + t(t(xi) %*% Cholesky)
    for(i in 1:d){
      xy[i,1] = xi[i]
      xy[i,2] = xi[i]
    }
  } else {
    e = z/normz
    utilde = runif(1);
    edotxi = sum(e*xi)
    if (log(utilde) < (-0.5*(edotxi+normz)*(edotxi+normz) + 0.5*edotxi*edotxi)){
      eta = xi + z
      identical = TRUE
    } else {
      eta = xi-2*edotxi%*%e
    }
    xi = mu1 + xi %*% Cholesky
    eta = mu2 + eta %*% Cholesky
    for(i in 1:d){
      xy[i,1] = xi[i]
      if (identical){
        xy[i,2] = xi[i]
      } else {
        xy[i,2] = eta[i];
      }
    }
  }
  return(list(xy = xy, identical=identical))
}



