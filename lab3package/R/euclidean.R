#' @title Euclidean algorithm
#' 
#' @description Computes the greatest common divisor of two integers.
#' 
#' @param a An integer.
#' @param b An integer.
#' 
#' @return The largest number that both numbers are divisible by.
#' 
#' @source \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' 
euclidean = function(a, b) {
  stopifnot(is.numeric(a), is.numeric(b), b > a)
  r = b %% a
  while (r) {
    b = a
    a = r
    r = b %% a
  }
  return(max(a, -a))
}
