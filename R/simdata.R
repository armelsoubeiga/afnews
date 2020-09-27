#' Simulate Multivariate Normal Distribution
#' @description Custom function to simulate multivariate normal distribution
#' @param n sample size (integer)
#' @param p number of variables (integer)
#' @param rho correlation between variables (between 0 and 1)
#' @return A nxp matrix of simulated data
#' @references
#' \url{https://gallery.rcpp.org/articles/simulate-multivariate-normal/}
#' @examples
#' dat <- mvrnormR(n = 100, p = 10, rho = 0.8)
#' heatmap(cor(dat))
#' @export
mvrnormR <- function(n, p, rho) {

  # covariance between Z_i and Z_j being rho^|i-j|
  times <- 1:p # used for creating covariance matrix
  H <- abs(outer(times, times, "-"))
  sigma <- rho^H

  mu <- rep(0, p)
  ncols <- ncol(sigma)
  mu <- rep(mu, each = n) ## not obliged to use a matrix (recycling)
  mu + matrix(stats::rnorm(n * ncols), ncol = ncols) %*% chol(sigma)
}
