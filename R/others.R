#' @export
outliers = function(s) {
  # find quantiles
  q = quantile(s, c(0.25, 0.75))
  # find standard deviation
  d = sd(s)

  res = c()

  for (i in 1:length(s)) {
    # check if the value is an outlier
    if (s[i] <= q[[1]] - 1.5 * d) {
      res = append(res, s[i])
    } else if (s[i] >= q[[2]] + 1.5 * d) {
      res = append(res, s[i])
    }
  }

  return(res)
}
