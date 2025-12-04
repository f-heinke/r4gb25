#' Title
#'
#' @param x
#' @param na.rm
#'
#' @return
#' @export sdN
#'
#' @examples
sdN <- function(x, na.rm = FALSE){
  return( sqrt( varN(x, na.rm) ))
}
