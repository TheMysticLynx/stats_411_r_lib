#' Find the mean of a class interval
#'
#' @param low Lower bound of the class interval
#' @param high Upper bound of the class interval
#' @param freq Frequency of the class interval
#' @return Mean of the class interval
#' @export
class_mean <- function(low, high, freq) {
  mid <- (low + high) / 2
  sum(mid * freq) / sum(freq)
}

#' Find the variance of a class interval
#'
#' @param low Lower bound of the class interval
#' @param high Upper bound of the class interval
#' @param freq Frequency of the class interval
#' @param pop Whether to use population or sample variance
#' @return Variance of the class interval
#' @export
class_var <- function(low, high, freq, pop = FALSE) {
  mid <- (low + high) / 2
  mean <- class_mean(low, high, freq)

  if (pop) {
    return(sum((mid - mean)^2 * freq) / sum(freq))
  } else {
    return(sum((mid - mean)^2 * freq) / (sum(freq) - 1))
  }
}

#' Find the standard deviation of a class interval
#'
#' @param low Lower bound of the class interval
#' @param high Upper bound of the class interval
#' @param freq Frequency of the class interval
#' @param pop Whether to use population or sample standard deviation
#' @return Standard deviation of the class interval
#' @export
class_sd <- function(low, high, freq, pop = FALSE) {
  sqrt(class_var(low, high, freq, pop))
}