# Download the data -------------------------------------------------------
require(tseries, quietly = T)
require(data.table)
library(zoo)
source('functions.R')
require(igraph)

sp.list <- fread('SPlist.csv')
sp.list$Sector <- as.factor(sp.list$Sector)
sectors <- levels(sp.list$Sector)

n_companies <- 3
data <- list()
sector.names <- c()
for(i in 1:length(sectors)){
  data[[i]] <- get.random.series(sp.list, sector = sectors[i], n = n_companies)
  sector.names <- c(sector.names, sectors[i])
}

names(data) <- sector.names

save(data, file='data_3.RData')

load(file = 'data_3.RData')
# Create data matrix ------------------------------------------------------

comp_name <- c()
for(li in data){
  comp_name <- c(comp_name, names(li[1:n_companies]))
}

X <- matrix()
X <- create_matrix(sectors, data)
colnames(X) <- comp_name
# Create MC graph ---------------------------------------------------------

# create pearson correlation matrix
R_hat <- cor(X,X)

conf_int <- bootstrap_procedure(X, R_hat = R_hat)

epsilon <- c(.05, .1, .2, .3, .4)
edges <- list()
for(i in 1:length(epsilon)){
  edges[[i]] <-  !(((conf_int[[1]] <= -epsilon[i]) & (-epsilon[i] <= conf_int[[2]])
               | (conf_int[[1]] <= epsilon[i]) & (epsilon[i] <= conf_int[[2]])))
}

plot.graph(edges[[1]], nstock = n_companies, nsectors = length(sectors))

plot.graph(edges[[2]])

plot.graph(edges[[3]])

plot.graph(edges[[4]])
# energy  -----------------------------------------------------------------

# use energy::dcov.test and use a bonferroni threshold
require(energy)

M <- matrix(NA, ncol = ncol(X), nrow = ncol(X))
diag(M) <- 1
for(i in 1:(ncol(X)-1)) {
  for(j in (i+1):ncol(X)) {
    dcov.i.j <- dcov.test(X[,i], X[,j], index=0.001, R=200) # nina said : enlarge R and shrink index ( ~ 0.001 )
    # Francesco said: provare con dcov.test
    M[i,j] <- M[j,i] <- dcov.i.j$p.value
  }
  print(i)
}

save(M, file='dcovMatrix.Rdata')
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

plot.graph(alpha.test, nstock = n_companies, nsectors = length(sectors))

# Bonferroni correction
bonf.test <- M < t_bonf
dimnames(bonf.test) <- dimnames(M)
plot.graph(bonf.test, nstock = n_companies, nsectors = length(sectors))

# Finally, if possible using
# same portfolio of stocks, grab data from January 1, 2013 through January 1, 2018. 
# Build the new matrix Y = y t,j t,j and repeat the previous analysis commenting 
# on observed differences between the two time-frames.

symbols = c()
for(i in 1:length(data)){
  symbols <- c(symbols, names(data[[i]]))
}

new.data <- list()
new.data <- get.series.symbols(symbols)
names(new.data)
# Create data matrix ------------------------------------------------------

new.comp_name <- c()
for(li in new.data){
  new.comp_name <- c(new.comp_name, names(li[1:n_companies]))
}

new.X <- matrix()
new.X <- create_matrix(sectors, data)
colnames(new.X) <- new.comp_name
# Create MC graph ---------------------------------------------------------

# create pearson correlation matrix
new.R_hat <- cor(new.X, new.X)

new.conf_int <- bootstrap_procedure(new.X, R_hat = new.R_hat)

epsilon <- c(.05, .1, .2, .3, .4)
new.edges <- list()
for(i in 1:length(epsilon)){
  new.edges[[i]] <-  !(((new.conf_int[[1]] <= -epsilon[i]) & (-epsilon[i] <= new.conf_int[[2]])
                    | (new.conf_int[[1]] <= epsilon[i]) & (epsilon[i] <= new.conf_int[[2]])))
}

plot.graph(new.edges[[1]], nstock = n_companies, nsectors = length(sectors))

plot.graph(new.edges[[2]])

plot.graph(new.edges[[3]])

plot.graph(new.edges[[4]])
# energy  -----------------------------------------------------------------

# use energy::dcov.test and use a bonferroni threshold
require(energy)

new.M <- matrix(NA, ncol = ncol(new.X), nrow = ncol(new.X))
diag(new.M) <- 1
for(i in 1:(ncol(new.X)-1)) {
  for(j in (i+1):ncol(new.X)) {
    dcov.i.j <- dcov.test(new.X[,i], new.X[,j], index=0.001, R=500) 
    new.M[i,j] <- new.M[j,i] <- dcov.i.j$p.value
  }
  print(i)
}

save(new.M, file='dcovnewMatrix_3.Rdata')
dimnames(new.M) <- list(colnames(new.X),colnames(new.X))

?dcor.test
# bonferroni threshold setup
alpha <- .05
n <- ncol(new.M)
m <- choose(n, 2)
t_bonf <- alpha / m

# awand a jidd
alpha.test <- new.M < alpha
dimnames(alpha.test) <- dimnames(new.M)

plot.graph(alpha.test, nstock = n_companies, nsectors = length(sectors))

# Bonferroni correction
bonf.test <- new.M < t_bonf
dimnames(bonf.test) <- dimnames(new.M)
plot.graph(bonf.test, nstock = n_companies, nsectors = length(sectors))

