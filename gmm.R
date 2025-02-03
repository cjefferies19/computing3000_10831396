# Load necessary libraries
library(MASS)
library(ggplot2)

# Sets the centre points and covariance for each cluster
mean1 <- c(0, 2)
sigma1 <- 0.8

mean2 <- c(1, -1)
sigma2 <- 1

mean3 <- c(-3, 4)
sigma3 <- 1.4

mean4 <- c(-1, -2)
sigma4 <- 0.7

# Determines the number of points each cluster has
pointNumbers <- c(1000, 1000, 1000, 1000)

# Calculating data clusters
data1 <- mvrnorm(pointNumbers[1], mean1, diag(sigma1, 2))
data2 <- mvrnorm(pointNumbers[2], mean2, diag(sigma2, 2))
data3 <- mvrnorm(pointNumbers[3], mean3, diag(sigma3, 2))
data4 <- mvrnorm(pointNumbers[4], mean4, diag(sigma4, 2))

# Combine data into one data frame for plotting
data <- rbind(data1, data2, data3, data4)
data <- as.data.frame(data)
data$cluster <- factor(rep(1:4, each = 1000))

# Plotting previously determined clusters into the same graph
ggplot(data, aes(x = V1, y = V2, color = cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Data Clusters")

# Gaussian Probability Density Function
gaussianProb <- function(data, mu, Sigma) {
  n <- ncol(data)
  meanDiff <- sweep(data, 2, mu)
  pd <- (1 / sqrt((2 * pi)^n * det(Sigma))) * exp(-0.5 * rowSums((meanDiff %*% solve(Sigma)) * meanDiff))
  return(pd)
}

# 100x100 grid
gridSize <- 100
bor <- seq(-8, 8, length.out = gridSize)
mGrid <- expand.grid(bor, bor)

# Calculates the density using gaussianProb
dense1 <- gaussianProb(mGrid, mean1, diag(sigma1, 2))
dense2 <- gaussianProb(mGrid, mean2, diag(sigma2, 2))
dense3 <- gaussianProb(mGrid, mean3, diag(sigma3, 2))
dense4 <- gaussianProb(mGrid, mean4, diag(sigma4, 2))

# Reshape for contour plot
con1 <- matrix(dense1, nrow = gridSize)
con2 <- matrix(dense2, nrow = gridSize)
con3 <- matrix(dense3, nrow = gridSize)
con4 <- matrix(dense4, nrow = gridSize)

# Contour plot
contour(bor, bor, con1, add = TRUE)
contour(bor, bor, con2, add = TRUE)
contour(bor, bor, con3, add = TRUE)
contour(bor, bor, con4, add = TRUE)

# Combining all data points into one array
dataA <- rbind(data1, data2, data3, data4)
dataPoints <- nrow(dataA)

# 4 clusters
clu <- 4
N <- nrow(dataA)
D <- ncol(dataA)

# Random permutation
indices <- sample(dataPoints)

# Initial means and covariance
mu <- dataA[indices[1:clu], ]
sigma <- vector("list", clu)
for(j in 1:clu) {
  sigma[[j]] <- cov(dataA)
}

# Cluster weights
phi <- rep(1 / clu, clu)
W <- matrix(0, dataPoints, clu)

# 1000 iteration for loop
performanceStore <- numeric(1000)
for(iter in 1:1000) {
  pdf <- matrix(0, dataPoints, clu)
  
  for(j in 1:clu) {
    pdf[, j] <- gaussianProb(dataA, mu[j, ], sigma[[j]])
  }
  
  pd_w <- sweep(pdf, 2, phi, `*`)
  W <- sweep(pd_w, 1, rowSums(pd_w), `/`)
  
  # Calculating performance metrics log likelihood
  performance <- sum(log(rowSums(pd_w)))
  performanceStore[iter] <- performance
  
  previousMu <- mu
  
  for(j in 1:clu) {
    phi[j] <- sum(W[, j]) / dataPoints
    mu[j, ] <- colSums(sweep(W[, j], 1, dataA, `*`)) / sum(W[, j])
    
    sigma_k <- matrix(0, D, D)
    data_m <- sweep(dataA, 2, mu[j, ])
    for(i in 1:dataPoints) {
      sigma_k <- sigma_k + W[i, j] * (t(data_m[i, ]) %*% data_m[i, ])
    }
    sigma[[j]] <- sigma_k / sum(W[, j])
  }
  
  if(sum((mu - previousMu)^2) < 1e-6) {
    break
  }
}

# New figure post GMM
ggplot(data, aes(x = V1, y = V2, color = cluster)) +
  geom_point() +
  geom_point(aes(x = mu[1, 1], y = mu[1, 2]), color = "black", shape = 4, size = 3) +
  geom_point(aes(x = mu[2, 1], y = mu[2, 2]), color = "black", shape = 4, size = 3) +
  geom_point(aes(x = mu[3, 1], y = mu[3, 2]), color = "black", shape = 4, size = 3) +
  geom_point(aes(x = mu[4, 1], y = mu[4, 2]), color = "black", shape = 4, size = 3) +
  theme_minimal() +
  labs(title = "GMM Data")

# Using updated covariance values new contours are created
dense1 <- gaussianProb(mGrid, mu[1, ], sigma[[1]])
dense2 <- gaussianProb(mGrid, mu[2, ], sigma[[2]])
dense3 <- gaussianProb(mGrid, mu[3, ], sigma[[3]])
dense4 <- gaussianProb(mGrid, mu[4, ], sigma[[4]])

# Reshape for contour plot
con1 <- matrix(dense1, nrow = gridSize)
con2 <- matrix(dense2, nrow = gridSize)
con3 <- matrix(dense3, nrow = gridSize)
con4 <- matrix(dense4, nrow = gridSize)

# Contour plot
contour(bor, bor, con1, add = TRUE)
contour(bor, bor, con2, add = TRUE)
contour(bor, bor, con3, add = TRUE)
contour(bor, bor, con4, add = TRUE)
