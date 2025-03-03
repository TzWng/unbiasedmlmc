% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_mh_kernels_beta.R
\name{get_mh_kernels_beta}
\alias{get_mh_kernels_beta}
\title{Get random walk Metropolis-Hastings kernels}
\usage{
get_mh_kernels_beta(target, Sigma_proposal)
}
\arguments{
\item{target}{function taking a vector as input and returning target log-density evaluation}

\item{Sigma_proposal}{covariance of the Normal random walk proposal}
}
\value{
A list containing the keys
\code{single_kernel}, \code{coupled_kernel}.
}
\description{
This function takes a beta distribution target (specified through its log-pdf)
and a covariance matrix for a Normal random walk proposal, and returns a list containing the keys
\code{single_kernel}, \code{coupled_kernel} corresponding to marginal
and coupled MH kernels.

The coupling is done by reflection-maximal coupling of the proposals,
and common uniform variable for the accept/reject step. For reflection-maximal
couplings, see \code{\link{rnorm_reflectionmax}} and \code{\link{rmvnorm_reflectionmax}}.
}
