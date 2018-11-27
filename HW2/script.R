# Download the data -------------------------------------------------------
require(tseries, quietly = T)
require(data.table)
library(zoo)

sp.list <- fread('SPlist.csv')
sp.list$Sector <- as.factor(sp.list$Sector)
sectors <- levels(sp.list$Sector)

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

data <- list()
sector.names <- c()
for(i in 1:length(sectors)){
  data[[i]] <- get_series(sp.list, sector = sectors[i])
  sector.names <- c(sector.names, sectors[i])
}
names(data) <- sector.names

save(data, file='data.RData')


# Create data matrix ------------------------------------------------------

X <- create_matrix(sectors, data)


# Create MC graph ---------------------------------------------------------

# create pearson correlation matrix
R_hat <- cor(X,X)
