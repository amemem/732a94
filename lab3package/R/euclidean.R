euclidean = function(a, b) {
  stopifnot(is.numeric(a), is.numeric(b), a > 0, b > a)
  remainder = a - ((a %/% b) * b)
  while (remainder) {
    a = b
    b = remainder
    remainder = a - ((a %/% b) * b)
  }
  return(b)
}

euclidean(123612, 13892347912)
euclidean(100, 1000)
