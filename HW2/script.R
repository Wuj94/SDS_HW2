# Download the data -------------------------------------------------------

require(tseries, quietly = T)
require(data.table)
library(zoo)
source('functions.R')
require(igraph)

# read csv containing companies with related symbols and sectors
sp.list <- fread('SPlist.csv')
sp.list$Sector <- as.factor(sp.list$Sector)
# pick the sectors names
sectors <- levels(sp.list$Sector)

# pick the quotes randomly (3 companies per sector)
n_companies <- 3
data <- list()
sector.names <- c()
for(i in 1:length(sectors)){
  sector.symbols <- sp.list[sp.list$Sector == sectors[i]]$Symbol
  data[[i]] <- get.random.series(sector.symbols, n = n_companies)
  sector.names <- c(sector.names, sectors[i])
}

names(data) <- sector.names

save(data, file='data_3.RData')
#load(file = 'data_3.RData')


# Create data matrix ------------------------------------------------------

comp_symbols <- c()
for(li in data){
  comp_symbols <- c(comp_symbols, names(li[1:n_companies]))
}

X <- matrix()
X <- create_matrix(sectors, data)
colnames(X) <- comp_symbols

X[1:5,1:5]
# Create MC graph ---------------------------------------------------------

# create pearson correlation matrix

conf_int <- bootstrap_procedure(X)

epsilon <- c(.1, .2, .3, .4)
edges <- list()
for(i in 1:length(epsilon)){
  edges[[i]] <-  !(((conf_int[[1]] <= -epsilon[i]) & (-epsilon[i] <= conf_int[[2]])
               | (conf_int[[1]] <= epsilon[i]) & (epsilon[i] <= conf_int[[2]])))
}

plot.graph(edges[[1]], nstock = n_companies, sectors=sectors)

plot.graph(edges[[2]], nstock = n_companies, sectors=sectors)

plot.graph(edges[[3]], nstock = n_companies, sectors=sectors)

plot.graph(edges[[4]], nstock = n_companies, sectors = sectors)


#plot.graph(edges[[4]], nstock = n_companies, nsectors = length(sectors))
# energy  -----------------------------------------------------------------

# use energy::dcov.test and use a bonferroni threshold
require(energy)

M <- matrix(NA, ncol = ncol(X), nrow = ncol(X))
diag(M) <- 1
for(i in 1:(ncol(X)-1)) {
  print(i)
  for(j in (i+1):ncol(X)) {
    dcov.i.j <- dcov.test(X[,i], X[,j], index=0.001, R=1000) # nina said : enlarge R and shrink index ( ~ 0.001 )
    # Francesco said: provare con dcov.test
    M[i,j] <- M[j,i] <- dcov.i.j$p.value
  }
}

save(M, file='dcovMatrix_5.Rdata')
dimnames(M) <- list(colnames(X),colnames(X))

?dcor.test
# bonferroni threshold setup
alpha <- .05
n <- ncol(M)
m <- choose(n, 2)
t_bonf <- alpha / m

# awand a jidd

alpha.test <- M < alpha
dimnames(alpha.test) <- dimnames(M)
#g <- graph_from_adjacency_matrix(adjmatrix = alpha.test, mode = c('undirected'), diag = 0)

plot.graph(alpha.test, nstock = n_companies, nsectors = length(sectors))

# Bonferroni correction
bonf.test <- M < t_bonf
dimnames(bonf.test) <- dimnames(M)
#g <- graph_from_adjacency_matrix(adjmatrix = alpha.test, mode = c('undirected'), diag = 0)

plot.graph(bonf.test, nstock = n_companies, nsectors = length(sectors))

