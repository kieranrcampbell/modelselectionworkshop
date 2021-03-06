
# Common functions to all models fitted

#' Calculate the mean vector given parameters and pseudotimes
#' 
#' This function (common to all models) calculates the sigmoidal mean vector
#' given the parameters and factor of pseudotimes
#' 
#' @param params Vector of length 4 with entries L = 2mu_0, k, t0, sig_sq
#' @param t Vector of pseudotimes
#' 
#' @return Mean sigmoidal vector
#' 
#' @export
calc_mu <- function(params, t) {
  L <- params[1] ; k <- params[2] ; t_0 <- params[3]
  mu <- L / (1 + exp(-k*(t - t_0)))
  return(mu)
}



#' Log-likelihood of iid measurements from a normal distribution
#' 
#' @param x A vector of measurements
#' @param mu A scalar mean
#' @param sig_sq a scalar variance
#' 
#' @return The log likelihood x given mu, sig_sq
log_norm_likelihood <- function(x, mu, sig_sq) {
  sum(dnorm(x, mu, sqrt(sig_sq), log = TRUE))
}



#' Negative log likelihood of the sigmoidal differential expression
#' function given some expression vector x
norm_alt_obj_func <- function(params, x, t) {
  sig_sq <- params[4]
  mu <- calc_mu(params, t)
  return( -log_norm_likelihood(x, mu, sig_sq) )
}