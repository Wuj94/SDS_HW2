# Download the data -------------------------------------------------------
require(tseries, quietly = T)
require(data.table)
library(zoo)

sp.list <- fread('SPlist.csv')
sp.list$Sector <- as.factor(sp.list$Sector)
sectors <- levels(sp.list$Sector)

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
conf_int <- bootstrap_procedure(X, R_hat = R_hat)

edges <- as.numeric((conf_int[[1]] <= R_hat) & (R_hat <= conf_int[[2]]))

