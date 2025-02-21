#' Calculate permutations, N choose R / N p R
#'
#' @param n The total number of items
#' @param r The number of items to choose
#' @return The number of permutations
#' @export
perm <- function(n, r) {
  if (n < r) {
    return("r cannot be greater than n!")
  }
  factorial(n) / factorial(n - r)
}

#' Calculate combinations, N choose R / N c R
#'
#' @param n The total number of items
#' @param r The number of items to choose
#' @return The number of combinations
#' @export
comb <- function(n, r) {
  factorial(n) / (factorial(r) * factorial(n - r))
}