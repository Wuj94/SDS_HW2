create_matrix <- function(sectors, data) {
  X.data = c()
  for(i in 1:length(sectors)){
    for(comp in data[[i]]){
      for(j in 2:length(comp)){
        X.data = c(X.data, log(comp[[j]]/comp[[j-1]]))
      }
    }
  }
  
  X = matrix(X.data, ncol = length(data) * length(data[[1]]))
  return(X)
}

bootstrap_proc <- function(X, corr_matrix, B = 1000, N = 1000){
  delta_b  <- list()
  for(i in B){
    indices <- sample(1:nrow(X), N)
    B_sample <- X[indices, ]
    B_cor <- cor(B_sample, B_sample)
    sim_stat <- sqrt(N) * max(abs(B_cor - corr_matrix))
    delta_b[[i]] <- sim_stat
  }
  distro <- ecdf(delta_b)
}