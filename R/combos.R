fact <- function(n) {
  if (n < 2) {
    return(1)
  }

  total <- 1
  for (i in 1:n) {
    total <- total * i
  }

  total
}