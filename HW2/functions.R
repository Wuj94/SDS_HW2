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

bootstrap_procedure <- function(X, R_hat, B = 1000, n = 400){
  delta_b <- c()
  for(i in 1:B){
    b_row_indices <- sample(1:nrow(X), n, replace = T)
    b_sample <- X[b_row_indices, ]
    b_R_hat <- cor(b_sample, b_sample)
    delta_b <- c(delta_b, sqrt(n) * max(abs(b_R_hat - R_hat)))
  }
  
  F_hat <- ecdf(delta_b)
  alpha <- 0.05
  t_alpha <- quantile(F_hat, 1-alpha)
  low_conf_int <- R_hat - t_alpha / sqrt(n)
  high_conf_int <- R_hat + t_alpha / sqrt(n)
  
  res <- list()
  res[[1]] <- low_conf_int
  res[[2]] <- high_conf_int
  return(res)
}

get_series <- function(sp.list, sector, n = 10) {
  n_series <- list()
  energy.symbols <- sp.list[sp.list$Sector == sector]$Symbol
  
  i <- 1
  added <- 0
  names.arr <- c()
  while(added < n && i <= length(energy.symbols)){
    result = tryCatch({
      a_series <- suppressWarnings(
        get.hist.quote(instrument = energy.symbols[i], start = '2003-01-01', end = '2008-01-01',
                       quote = c('Close'), provider = 'yahoo', drop = T) )
      if(start(a_series) == '2003-01-02' && end(a_series) == '2007-12-31') {
        added <- added + 1
        n_series[[added]] <- a_series
        names.arr <- c(names.arr, energy.symbols[i])
      }
    }, error = function(e) {
      print("error")
    })
    i <- i + 1
  }
  names(n_series) <- names.arr
  return(n_series)
}