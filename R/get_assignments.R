get_assignments <- function(X,W){
  sapply(1:nrow(X), function(k){
    x_k <- X[k,]
    diffs <-  sweep(W,MARGIN = 2,STATS = x_k,FUN= "-")#t(t(W) - x_k)
    dists <- rowSums(diffs^2)
    which.min(dists)
  })
}
