#' @export km_scree_test
km_scree_test <- function(X, K, N_replications = 32, init_method = kmpp_init){

  ssrat_result <- list()

  ssrat_stats <- data.frame(k = numeric(0), mean_ssratio = numeric(0), sd_ssratio = numeric(0))

  for(k in K){
    kid <- paste0("k", k)
    ssrat_result[[ kid ]] <- numeric()

    for(i in 1:N_replications){

      W <- init_method(X = X, k = k)
      km <- kmeans(x = X, centers = W)
      ssrat <- km$withinss / km$betweenss
      ssrat_result[[kid]] <- append(ssrat_result[[kid]], ssrat )
    }

    ssrat_stats <- rbind( ssrat_stats, c(k , mean( ssrat_result[[kid]] ), sd( ssrat_result[[kid]] )))

  }
  colnames( ssrat_stats ) <- c("k", "mean_ssratio", "sd_ssratio")

  scree_test_result <- list(
    stats = ssrat_stats,
    ssratios = ssrat_result
  )
  class(scree_test_result) <- "km_scree_test"
  return( scree_test_result )
}

#' @export km_rand_init
km_rand_init <- function(X, k){
  X[ sample(1:nrow( X ), k), ]
}

#' @export plot.km_scree_test
plot.km_scree_test <- function( kmst ){
  d <- kmst$ssratios
  boxplot(d)
}


