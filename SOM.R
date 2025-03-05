#### Load data from CSV file ####
new_data <- read.csv("newData.csv")

#### Ensure the data contains only RGB values ####
numeric_data <- new_data[sapply(new_data, is.numeric)]
numeric_data <- numeric_data[, 1:3]  # Ensure only three columns for RGB
colnames(numeric_data) <- c('R', 'G', 'B')

#### Train the SOM ####

library(kohonen)

sample.rgb <- numeric_data  # Use data from CSV instead of generating random values
sample.size <- nrow(sample.rgb)

grid.size <- ceiling(sample.size ^ (1/2.5))
som.grid <- somgrid(xdim = grid.size, ydim = grid.size, topo = 'hexagonal', toroidal = T)
som.model <- som(data.matrix(sample.rgb), grid = som.grid)

## Extract some data to make it easier to use
som.events <- som.model$codes[[1]]
som.events.colors <- rgb(som.events[,1], som.events[,2], som.events[,3], maxColorValue = 255)
som.dist <- as.matrix(dist(som.events))

## Generate a plot after training.
plot(som.model,
     type = 'mapping',
     bg = som.events.colors,
     keepMargins = F,
     col = NA,
     main = '')

#### Look for a reasonable number of clusters ####

clusterMeanDist <- function(clusters){
  cluster.means = c()
  
  for(c in unique(clusters)){
    temp.members <- which(clusters == c)
    
    if(length(temp.members) > 1){
      temp.dist <- som.dist[temp.members,]
      temp.dist <- temp.dist[,temp.members]
      cluster.means <- append(cluster.means, mean(temp.dist))
    }else(cluster.means <- 0)
  }
  
  return(mean(cluster.means))
  
}

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

#### Evaluate clustering algorithms ####

library(pmclust)

plotSOM <- function(clusters){
  plot(som.model,
       type = 'mapping',
       bg = som.events.colors,
       keepMargins = F,
       col = NA)
  
  add.cluster.boundaries(som.model, clusters)
}

cluster.tries <- list()

for(k in c(20)){
  
  ## Model-based clustering using pmclust
  som.cluster.pm.em <- pmclust(som.events, K = k, algorithm = 'em')$class
  som.cluster.pm.aecm <- pmclust(som.events, K = k, algorithm = 'aecm')$class
  som.cluster.pm.apecm <- pmclust(som.events, K = k, algorithm = 'apecm')$class
  som.cluster.pm.apecma <- pmclust(som.events, K = k, algorithm = 'apecma')$class
  som.cluster.pm.kmeans <- pmclust(som.events, K = k, algorithm = 'kmeans')$class
  
  ## k-means clustering
  som.cluster.k <- kmeans(som.events, centers = k, iter.max = 100, nstart = 10)$cluster
  
  ## Hierarchical clustering
  som.dist <- dist(som.events)
  som.cluster.h <- cutree(hclust(som.dist), k = k)
  
  ## Capture outputs
  cluster.tries[[paste0('som.cluster.pm.em.', k)]] <- som.cluster.pm.em
  cluster.tries[[paste0('som.cluster.pm.aecm.', k)]] <- som.cluster.pm.aecm
  cluster.tries[[paste0('som.cluster.pm.apecm.', k)]] <- som.cluster.pm.apecm
  cluster.tries[[paste0('som.cluster.pm.apecma.', k)]] <- som.cluster.pm.apecma
  cluster.tries[[paste0('som.cluster.pm.kmeans.', k)]] <- som.cluster.pm.kmeans
  cluster.tries[[paste0('som.cluster.k.', k)]] <- som.cluster.k
  cluster.tries[[paste0('som.cluster.h.', k)]] <- som.cluster.h
}

## Take a look at the various clusters
plotSOM(cluster.tries$som.cluster.pm.em.20)
plotSOM(cluster.tries$som.cluster.pm.aecm.20)
plotSOM(cluster.tries$som.cluster.pm.apecm.20)
plotSOM(cluster.tries$som.cluster.pm.apecma.20)
plotSOM(cluster.tries$som.cluster.k.20)
plotSOM(cluster.tries$som.cluster.h.20)
