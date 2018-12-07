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

# implements the bootstrap procedure described in the notes
# input parameters:
# - X: data matrix (D stocks and n observation)
# - B: number of bootstrap replications
# - n: size of the samples (rows picked randomly from X)
# - alpha: level of the confidence intervals
# output: 
# -list containing two matrices:
##  [0]: lower bounds of the CI's
##  [0]: upper bounds of the CI's

bootstrap_procedure <- function(X, B = 1000, n = 400, alpha = 0.05){
  
  # sample correlation matrix
  R_hat <- cor(X,X)
  
  # bootstrapped replicates of simultaneous test statistics
  delta_b <- c()
  
  # bootstrap procedure
  for(i in 1:B){
    # matrix sampled from X
    b_row_indices <- sample(1:nrow(X), n, replace = T)
    b_sample <- X[b_row_indices, ]
    
    # bootstrap correlation matrix
    b_R_hat <- cor(b_sample, b_sample)
    delta_b <- c(delta_b, sqrt(ncol(X)) * max(abs(b_R_hat - R_hat)))
  }
  
  # ECDF of the statistics
  F_hat <- ecdf(delta_b)
  # sample quantile (at level 1-alpha)
  t_alpha <- quantile(F_hat, 1-alpha)
  
  # lower and upper bounds
  low_conf_int <- R_hat - t_alpha / sqrt(ncol(X)) 
  high_conf_int <- R_hat + t_alpha / sqrt(ncol(X)) 
  
  res <- list()
  res[[1]] <- low_conf_int
  res[[2]] <- high_conf_int
  return(res)
}

# plot graph
plot.graph <- function(edges, nstock = 3, sectors){
  #nsectors <- length(sectors)
  g <- graph_from_adjacency_matrix(adjmatrix = edges, mode = c('undirected'), diag = 0)
  
  colors = c("green", "blue", "orchid", "orange", "yellow", "antiquewhite",
             "chartreuse4", "cyan4", "red", "darkgoldenrod3", "khaki")
  
  layout <- layout.auto
  
  V(g)$color = rep(colors, each=nstock)
 
  plot(g, edges.arrow.size = 5, vertex.size=20, vertex.label.dist = 2,
       layout=layout, edge.width = 1.5)
  
  legend(x = 'topleft', sectors, col=colors, bty = 'n', cex=0.6, pch=16)
  
#  legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
#         col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
  
  
}



# 1. get data

# 1. Get data -------------------------------------------------------------



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


# 1.b get random data


get.random.series <- function(sector.symbols, n = 10) {

  set.seed(1234)
  n_series <- list()
  
  i = 0
  cnt = 0
  symbols.list <- c()
  names.arr <- c()
  while ((i < n) & (cnt < length(sector.symbols))){
    
    random.sym = sample(sector.symbols, size=1)
    
    result = tryCatch({
      if (!(random.sym %in% symbols.list)){
        symbols.list <- c(symbols.list, random.sym)
        cnt <- cnt + 1
        a_series <- suppressWarnings(
          get.hist.quote(instrument = random.sym, start = '2003-01-01', end = '2008-01-01',
                         quote = c('Close'), provider = 'yahoo', drop = T) )
        if((start(a_series) == '2003-01-02') && (end(a_series) == '2007-12-31')) {
          n_series[[i+1]] <- a_series
          names.arr <- c(names.arr, random.sym)
          i <- i + 1
        }
      }
    }, error = function(e) {
      print(e)
    })

  }
  
  names(n_series) <- names.arr
  return (n_series)
}
  