#' Center metric data matrix by column means.
#'
#' @param X A complete (N x n) metric data matrix of N obs units and n features.
#'
#' @returns Centered data matrix.
#' @export
#'
#' @examples
center <- function(X){
  return( sweep(X, 2, colMeans(X)) )
}
