#' Find the mean of a Bernoulli distribution
#'
#' @param p Probability of success
#' @return Mean of the Bernoulli distribution
#' @export
ber_mean <- function(p) {
  p
}

#' Find the variance of a Bernoulli distribution
#'
#' @param p Probability of success
#' @return Variance of the Bernoulli distribution
#' @export
ber_var <- function(p) {
  p * (1 - p)
}

#' Find the standard deviation of a Bernoulli distribution
#'
#' @param p Probability of success
#' @return Standard deviation of the Bernoulli distribution
#' @export
ber_sd <- function(p) {
  sqrt(ber_var(p))
}

#' Find the mean of a uniform distribution
#'
#' @param x set of values
#' @return Mean of the uniform distribution
#' @export
uniform_mean <- function(x) {
  sum(x / length(x))
}

#' Find the variance of a uniform distribution
#'
#' @param x set of values
#' @return Variance of the uniform distribution
#' @export
uniform_var <- function(x) {
  mean <- uniform_mean(x)
  sum(x^2) / length(x) - mean^2
}

#' Find the standard deviation of a uniform distribution
#'
#' @param x set of values
#' @return Standard deviation of the uniform distribution
#' @export
uniform_sd <- function(x) {
  sqrt(uniform_var(x))
}

#' Find the mean of a discrete distribution
#'
#' @param val Values of the discrete distribution
#' @param freq Frequency of the discrete distribution
#' @return Mean of the discrete distribution
#' @export
discrete_mean <- function(val, freq) {
  sum(val * freq) / sum(freq)
}

#' Find the variance of a discrete distribution
#'
#' @param val Values of the discrete distribution
#' @param freq Frequency of the discrete distribution
#' @return Variance of the discrete distribution
#' @export
discrete_var <- function(val, freq) {
  mean <- discrete_mean(val, freq)
  sum(val^2 * freq) - mean^2
}

#' Find the standard deviation of a discrete distribution
#'
#' @param val Values of the discrete distribution
#' @param freq Frequency of the discrete distribution
#' @return Standard deviation of the discrete distribution
#' @export
discrete_sd <- function(val, freq) {
  sqrt(discrete_var(val, freq))
}