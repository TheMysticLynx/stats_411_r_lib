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
#' @param min Minimum value of the uniform distribution
#' @param max Maximum value of the uniform distribution
#' @return Mean of the uniform distribution
#' @export
#'
#' @examples
#' unif_mean(5, 15)
#' unif_mean(1, 10)
unif_mean <- function(min, max) {
  (min + max) / 2
}

#' Find the variance of a uniform distribution
#'
#' @param min Minimum value of the uniform distribution
#' @param max Maximum value of the uniform distribution
#' @return Variance of the uniform distribution
#' @export
#'
#' @examples
#' unif_var(1, 10)
#' unif_var(5, 15)
unif_var <- function(min, max) {
  ((max - min)^2) / 12
}

#' Find the standard deviation of a uniform distribution
#'
#' @param min Minimum value of the uniform distribution
#' @param max Maximum value of the uniform distribution
#' @return Standard deviation of the uniform distribution
#' @export
#'
#' @examples
#' unif_sd(1, 10)
#' unif_sd(5, 15)
unif_sd <- function(min, max) {
  (max - min) / sqrt(12)
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

#' Calculate expected value for a discreet random variable given a PDF
#'
#' NOTE - Length of x must equal lenght of prob, and prob must sum to 1
#'
#' @param x The set of all possilbe values
#' @param r The set of probabilities for all expected values
#' @return The expected value (mean)
#' @export
drvmean <- function(x, prob) {
  if (length(x) != length(prob)) {
    return("X set and Probability sets must have the same length")
  }

  if (sum(prob) != 1) {
    return("Sum of Probabilty set must equal 1")
  }

  mean_val <- sum(x * prob)
  mean_val
}


#' Calculate variance for a discreet random variable given a PDF
#'
#' NOTE - Length of x must equal lenght of prob, and prob must sum to 1
#'
#' @param x The set of all possilbe values
#' @param r The set of probabilities for all expected values
#' @return The variance for the sets
#' @export
drvvar <- function(x, prob) {
  if (length(x) != length(prob)) {
    return("X set and Probability sets must have the same length")
  }

  if (sum(prob) != 1) {
    return("Sum of Probabilty set must equal 1")
  }

  mean_val <- drvmean(x, prob)
  var_val <- sum((x^2) * prob)
  var_val <- var_val - mean_val^2

  var_val
}


#' Calculate standard deviation for a discreet random variable given a PDF
#'
#' NOTE - Length of x must equal lenght of prob, and prob must sum to 1
#'
#' @param x The set of all possilbe values
#' @param r The set of probabilities for all expected values
#' @return The standard deviation for the sets
#' @export
drvdev <- function(x, prob) {
  dev <- drvvar(x, prob)
  dev <- sqrt(dev)

  dev
}
