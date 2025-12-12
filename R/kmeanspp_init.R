#' Simple implementation of k-means++ initialized prototypes. Utilizes sqrd. euclidean distance!
#'
#' @param X The (N x n) data matrix
#' @param k Number of prototypes
#'
#' @returns A (K x n) matrix of prototypes
#' @export
#'
#' @examples
kmpp_init <- function(X, k = 3){
  N <- nrow(X)
  W <- matrix(nc = ncol(X), nr = k)

  idx_all <- 1:N
  idx_w <- numeric()

  weights <- rep(1, N)

  for(i in 1:k){
    idx_selectable <- setdiff(idx_all, idx_w)
    idx_next_w <- sample(idx_selectable,1, prob = weights[idx_selectable])
    idx_w <- append(idx_w, idx_next_w)

    W <- X[idx_w, ]
    if(i == 1){
      weights <- rowSums(sweep(X,2,W)^2)
    }else{
      assigns <- get_assignments(X,W)
      dists <- sapply(1:N, function(i){
        sum((W[assigns[i],] - X[i,])^2)
      })
      weights <- dists
    }
  }

  return(W)
}
