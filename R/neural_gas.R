neural_gas <- function(X, K = NULL, W =  NULL, lambda_init = NULL, lambda_stop = 0.001, learn_rate = 0.1, max_iter = nrow(X) * 100){
  N <- nrow(X)
  n <- ncol(X)

  if(is.null(K)){
    K <- ceiling(N / 20) # K := number of weights/prototypes

  }

  if(is.null(W)){
    W <- matrix(nr = K, nc = n, data = rnorm(K *n))

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
    print(paste(t, "/",max_iter))

  }
  return(W)
}
