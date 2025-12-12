#' z-scaling for some vector using "geometric" standard deviation
#'
#' @param x The data vector
#' @param center logical. Invoke data centering.
#' @param scale logical. Invoke data scaling by factor 1/N, instead of Bessel correction.
#'
#' @returns Scaled data vector.
#' @export
#'
#' @examples
scaleN <- function(x, center = T, scale = T){
  cx <- ifelse(center, mean(x), 0)
  sx <- ifelse(scale,  sdN(x), 1)

  z <- (x - cx) / sx
  if(center) attr(z, "scaled:center") <- cx
  if(scale)  attr(z, "scaled:scale")  <- sx
  return(z)
}
