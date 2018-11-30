# Download the data -------------------------------------------------------
require(tseries, quietly = T)
require(data.table)
library(zoo)

sp.list <- fread('SPlist.csv')
sp.list$Sector <- as.factor(sp.list$Sector)
sectors <- levels(sp.list$Sector)

n_companies <- 5
data <- list()
sector.names <- c()
for(i in 1:length(sectors)){
  data[[i]] <- get_series(sp.list, sector = sectors[i], n = n_companies)
  sector.names <- c(sector.names, sectors[i])
}
names(data) <- sector.names

save(data, file='data_5.RData')

load(file = 'data.RData')
# Create data matrix ------------------------------------------------------

comp_name <- c()
for(li in data){
  comp_name <- c(comp_name, names(li[1:n_companies]))
}

X <- create_matrix(sectors, data)
colnames(X) <- comp_name

# Create MC graph ---------------------------------------------------------

# create pearson correlation matrix
R_hat <- cor(X,X)

conf_int <- bootstrap_procedure(X, R_hat = R_hat)

epsilon <- .5
edges <- ((conf_int[[1]] <= -epsilon) & (-epsilon <= conf_int[[2]])
   | (conf_int[[1]] <= epsilon) & (epsilon <= conf_int[[2]]))

require(igraph)
g <- graph_from_adjacency_matrix(adjmatrix = edges, mode = c('undirected'), diag = 0)
V(g)[1:5]$color <- "green"
V(g)[6:10]$color <- "blue"
V(g)[11:15]$color <- "orchid"
V(g)[16:20]$color <- "orange"
V(g)[21:25]$color <- "yellow"
V(g)[26:30]$color <- "red"
V(g)[31:35]$color <- "antiquewhite"
V(g)[36:40]$color <- "chartreuse4"
V(g)[41:45]$color <- "cyan4"
V(g)[46:50]$color <- "darkgoldenrod3"
V(g)[51:55]$color <- "khaki"
plot(g, rescale = F, ylim=c(-5, 5), xlim=c(-6,6) ,vertex.size=20, axes=T)
