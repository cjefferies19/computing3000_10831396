#### Load Data from CSV ####

## Load necessary library
library(data.table)

## Read the data from CSV
newData <- fread("newData.csv")

#### train the SOM ####

## define a grid for the SOM and train

library(kohonen)

grid.size <- ceiling(nrow(newData) ^ (1/2.5))
som.grid <- somgrid(xdim = grid.size, ydim = grid.size, topo = 'hexagonal', toroidal = T)
som.model <- som(data.matrix(newData), grid = som.grid)

## extract some data to make it easier to use

som.events <- som.model$codes[[1]]
som.dist <- as.matrix(dist(som.events))

## generate a plot after training.

plot(som.model,
     type = 'mapping',
     keepMargins = F,
     col = NA,
     main = '')

#### look for a reasonable number of clusters ####

try.k <- 2:100
cluster.dist.eval <- as.data.frame(matrix(ncol = 3, nrow = (length(try.k))))
colnames(cluster.dist.eval) <- c('k', 'kmeans', 'hclust')

for(i in 1:length(try.k)) {
  cluster.dist.eval[i, 'k'] <- try.k[i]
  cluster.dist.eval[i, 'kmeans'] <- clusterMeanDist(kmeans(som.events, centers = try.k[i], iter.max = 20)$cluster)
  cluster.dist.eval[i, 'hclust'] <- clusterMeanDist(cutree(hclust(vegdist(som.events)), k = try.k[i]))
}

plot(cluster.dist.eval[, 'kmeans'] ~ try.k,
     type = 'l')

lines(cluster.dist.eval[, 'hclust'] ~ try.k,
      col = 'red')

legend('topright',
       legend = c('k-means', 'hierarchical'),
       col = c('black', 'red'),
       lty = c(1, 1))

#### evaluate clustering algorithms ####

library(pmclust)

plotSOM <- function(clusters){
  plot(som.model,
       type = 'mapping',
       keepMargins = F,
       col = NA)
  
  add.cluster.boundaries(som.model, clusters)
}

cluster.tries <- list()

for(k in c(20)){
  
  som.cluster.pm.em <- pmclust(som.events, K = k, algorithm = 'em')$class
  som.cluster.pm.aecm <- pmclust(som.events, K = k, algorithm = 'aecm')$class
  som.cluster.pm.apecm <- pmclust(som.events, K = k, algorithm = 'apecm')$class
  som.cluster.pm.apecma <- pmclust(som.events, K = k, algorithm = 'apecma')$class
  som.cluster.pm.kmeans <- pmclust(som.events, K = k, algorithm = 'kmeans')$class
  
  som.cluster.k <- kmeans(som.events, centers = k, iter.max = 100, nstart = 10)$cluster
  
  som.dist <- dist(som.events)
  som.cluster.h <- cutree(hclust(som.dist), k = k)
  
  cluster.tries[[paste0('som.cluster.pm.em.', k)]] <- som.cluster.pm.em
  cluster.tries[[paste0('som.cluster.pm.aecm.', k)]] <- som.cluster.pm.aecm
  cluster.tries[[paste0('som.cluster.pm.apecm.', k)]] <- som.cluster.pm.apecm
  cluster.tries[[paste0('som.cluster.pm.apecma.', k)]] <- som.cluster.pm.apecma
  cluster.tries[[paste0('som.cluster.pm.kmeans.', k)]] <- som.cluster.pm.kmeans
  cluster.tries[[paste0('som.cluster.k.', k)]] <- som.cluster.k
  cluster.tries[[paste0('som.cluster.h.', k)]] <- som.cluster.h
}

plotSOM(cluster.tries$som.cluster.pm.em.20)
plotSOM(cluster.tries$som.cluster.pm.aecm.20)
plotSOM(cluster.tries$som.cluster.pm.apecm.20)
plotSOM(cluster.tries$som.cluster.pm.apecma.20)
plotSOM(cluster.tries$som.cluster.k.20)
plotSOM(cluster.tries$som.cluster.h.20)
