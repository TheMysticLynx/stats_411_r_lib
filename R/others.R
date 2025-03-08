#' Find outliers
#'
#' @param Set of numbers
#' @return A set of outliers
#' @export
outliers <- function(s) {
  # find quantiles
  q <- quantile(s, c(0.25, 0.75))
  # find standard deviation
  d <- sd(s)

  res <- c()

  for (i in seq_along(s)) {
    # check if the value is an outlier
    if (s[i] <= q[[1]] - 1.5 * d) {
      res <- append(res, s[i])
    } else if (s[i] >= q[[2]] + 1.5 * d) {
      res <- append(res, s[i])
    }
  }

  res
}

#' Find population variance
#'
#' @param Set of numbers
#' @param Whether to use population or sample variance
#' @return Population variance
#' @export
pop_var <- function(s, pop = TRUE) {
  m <- mean(s)

  if (pop) {
    return(sum((s - m)^2) / length(s))
  } else {
    return(sum((s - m)^2) / (length(s) - 1))
  }
}

#' Find population standard deviation
#'
#' @param Set of numbers
#' @param Whether to use population or sample standard deviation
#' @return Population standard deviation
#' @export
pop_sd <- function(s, pop = TRUE) {
  return(sqrt(pop_var(s, pop)))
}

#' Find population coefficient of variation
#'
#' @param Set of numbers
#' @param Whether to use population or sample coefficient of variation
#' @return Population coefficient of variation
#' @export
pop_cv <- function(s, pop = TRUE) {
  pop_sd(s) / mean(s) * 100
}

#' Prints the imperical rule
#'
#' The imperical rule states that for a normal distribution:
#' - 68.26% of the data falls within 1 standard deviation of the mean
#' - 95.44% of the data falls within 2 standard deviations of the mean
#' - 99.74% of the data falls within 3 standard deviations of the mean
imp <- function() {
  c(68.26, 95.44, 99.74)
}