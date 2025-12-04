get_data_clusters <- function(X,W,assigns = get_assignments(X,W)){
  sapply(1:nrow(W),function(j){
    X[which(assigns == j),]
  })
}
