#' Compute "geometric" standard deviation, without Bessel correction.
#'
#' @param x Vector containing metric data.
#' @param na.rm Logical; remove NA data prior to computations. Defaults to FALSE.
#'
#' @return Scalar value.
#' @export sdN
#'
#' @examples
sdN <- function(x, na.rm = FALSE){
  return( sqrt( varN(x, na.rm) ))
}
