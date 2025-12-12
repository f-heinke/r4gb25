#' Title
#'
#' @param X Design or raw data (N x n) matrix
#' @param K Number of prototypes to compute, defaults to ceiling(nrow(X) / 20)
#' @param W Optional (K x n) matrix of initial prototype data. If given, the K argument is ignored
#' @param lambda_init Initial general adaption rate
#' @param lambda_stop Target adaption rate during last adaption step. Default is 0.001.
#' @param learn_rate Constant adaption rate. Default is 0.1
#' @param max_iter The number of steps to perform. Defaults to N x 100.
#' @param verbose Should outputs be printed. Default is TRUE.
#'
#' @returns (K x n) matrix of prototype data after learning
#' @export
#'
#' @examples
neural_gas <- function(X, K = ceiling(nrow(X) / 20), W =  NULL, lambda_init = NULL, lambda_stop = 0.001, learn_rate = 0.1, max_iter = nrow(X) * 100, verbose = TRUE){
  N <- nrow(X)
  n <- ncol(X)

  if(is.null(W)){
    W <- X[ sample(1:N, size = K), ]
  }
  if(is.null(lambda_init)){
    lambda_init  <- K / 3
  }

  delta <- exp( log(lambda_stop / lambda_init) / max_iter  )
  lambda <- lambda_init


  for(t in 1:max_iter){
    k <- sample(1:N, 1)
    x_k <- as.matrix(X[k,])


    #diffs <-  t(t(W) - x_k)
    diffs <- sweep(W,MARGIN = 2, STATS = x_k, FUN = "-")
    dists <- rowSums(diffs^2)


    r <- rank(dists) - 1
    h <- exp(- r / lambda)

    W <- t(sapply(1:K, function(j){
      w_j <- W[j,]
      return(w_j + learn_rate * h[j] * (x_k - w_j) ) # Apply the NG adaption scheme to  w_j
    }))



    # After that you can decrease lambda ....

    lambda <- lambda * delta
    if( verbose ) print(paste(t, "/",max_iter))

  }
  return(W)
}
