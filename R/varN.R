#' Title
#'
#' @param x
#' @param na.rm
#'
#' @return
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
