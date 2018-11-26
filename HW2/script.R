# Download the data -------------------------------------------------------
require(tseries, quietly = T)
require(data.table)


sp.list <- fread('SPlist.csv')
sp.list$Sector <- as.factor(sp.list$Sector)
sectors <- levels(sp.list$Sector)

get_series <- function(sp.list, sector, n = 10) {
  n_series <- list()
  energy.symbols <- sp.list[sp.list$Sector == sector]$Symbol
  
  i <- 1
  added <- 0
  while(added < n && i <= length(energy.symbols)){
    result = tryCatch({
      a_series <- suppressWarnings(
        get.hist.quote(instrument = energy.symbols[i], start = '2003-01-01', end = '2008-01-01',
                       quote = c('Close'), provider = 'yahoo', drop = T) )
      if(start(a_series) == '2003-01-01' && end(a_series) == '2007-12-31') {
        n_series[[i]] <- a_series
        added <- added + 1
      }
    }, error = function(e) {
      print("error")
    })
    i <- i + 1
  }
  return(n_series)
}

series <- get_series(sp.list, sector = sectors[1])
