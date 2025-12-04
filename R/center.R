#'
#'
#'
#'
#' @export center
#'
#' @examples
center <- function(X){
  return( sweep(X, 2, colMeans(X)) )
}
