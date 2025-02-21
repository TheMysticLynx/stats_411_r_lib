




perm = function(n, k) {
  if (k == 0) {
    return (1)
  }

  if (k == 1) {
    return (n)
  }

  return (fact(n) / fact(n - k))
}

comb = function(n, k) {
  if (k == 0) {
    return (1)
  }

  if (k == 1) {
    return (n)
  }

  return (fact(n) / (fact(k) * fact(n - k)))
}