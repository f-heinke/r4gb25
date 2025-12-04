#' Performs classic principal component "analysis" - or objects usable for analysis, at least
#'
#' @param X
#' @param center
#' @param scale
#' @param k
#'
#' @return
#' @export pca
#'
#' @examples
pca <- function(X, center = T, scale = T, k = 2){
  if(ncol(X) > nrow(X)){
    stop("Cannot compute PCA for data with smaller number of observations than variables.")
  }
  D <- as.matrix( scale(X, center = center, scale = scale) )
  C <- cov(D)
  E <- eigen(C)
  P <- E$vectors
  v <- E$values

  Sn <- t(t(P) %*% t(D))
  S <- Sn[,1:k]
  #test
  rel.var <- v / sum(v)
  rel.var.perc <- 100 * v / sum(v)
  cumul.rel.var <- cumsum(rel.var)
  cumul.rel.var.perc <- cumsum(rel.var.perc)
  return(list(
    scores = S,
    scores.n = Sn,
    rel.var = rel.var,
    rel.var.perc = rel.var.perc,
    cumul.rel.var = cumul.rel.var,
    cumul.rel.var.perc = cumul.rel.var.perc,
    eigen.vectors = P,
    eigen.values = v,
    k = k,
    D = D
  ))
}
