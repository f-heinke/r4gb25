#' Compute "geometric" variance, without Bessel correction.
#'
#'
#' @param x Vector containing metric data.
#' @param na.rm Logical; remove NA data prior to computations. Defaults to FALSE.
#'
#' @return A scalar value
#' @export varN
#'
#' @examples
varN <- function(x, na.rm = FALSE){

  if(na.rm == T){
    x <- x[-which(is.na(x))]
  }

  N <- length(x)

  if(N <= 1){
    return(NA)
  }

  v <- var(x)

  v2 <- (N-1)/N * v

  return(v2)

}
