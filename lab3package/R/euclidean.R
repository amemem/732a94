#' @title Euclidean algorithm
#' 
#' @description Computes the greatest common divisor of two positive integers.
#' 
#' @param a An integer.
#' @param b An integer.
#' 
#' @return The largest number that both numbers are divisible by.
#' 
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' 
#' @source \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' 
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
