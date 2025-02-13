#' @export
class_mean = function(low, high, freq) {
  mid = (low + high) / 2
  return (sum(mid * freq) / sum(freq))
}

#' @export
class_var = function(low, high, freq, pop = FALSE) {
  mid = (low + high) / 2
  mean = class_mean(low, high, freq)

  if (pop) {
    return (sum((mid - mean)^2 * freq) / sum(freq))
  } else {
    return (sum((mid - mean)^2 * freq) / (sum(freq) - 1))
  }
}

#' @export
class_sd = function(low, high, freq, pop = FALSE) {
  return (sqrt(class_var(low, high, freq, pop)))
}
