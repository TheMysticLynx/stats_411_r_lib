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
