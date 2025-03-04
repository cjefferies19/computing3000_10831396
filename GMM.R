library(keras)
library(MASS)
library(ggplot2)

# Load the pre-trained autoencoder
encoder <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(784)) %>%
  layer_dense(units = 2, activation = 'relu')

decoder <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(2)) %>%
  layer_dense(units = 784, activation = 'sigmoid')

autoencoder <- keras_model(inputs = encoder$input, outputs = decoder(encoder$output))

# Compile the autoencoder
autoencoder %>% compile(optimizer = 'adam', loss = 'binary_crossentropy')

# Load and preprocess dataset
new_data <- as.matrix(big_data)
new_data <- scale(big_data) # Normalise the data

# Generate the encoded (latent) representation
encoded_data <- encoder %>% predict(big_data)

dataA <- encoded_data  # GMM input

dataPoints <- nrow(dataA)
clu <- 4  # Set number of clusters
N <- nrow(dataA)
D <- ncol(dataA)

# Initialize parameters
indices <- sample(dataPoints)
mu <- dataA[indices[1:clu], ]
sigma <- vector("list", clu)
for(j in 1:clu) {
  sigma[[j]] <- cov(dataA)
}
phi <- rep(1 / clu, clu)
W <- matrix(0, dataPoints, clu)

# Gaussian Probability Function
gaussianProb <- function(data, mu, Sigma) {
  n <- ncol(data)
  meanDiff <- sweep(data, 2, mu)
  pd <- (1 / sqrt((2 * pi)^n * det(Sigma))) * exp(-0.5 * rowSums((meanDiff %*% solve(Sigma)) * meanDiff))
  return(pd)
}

# Training Loop
performanceStore <- numeric(1000)
for(iter in 1:1000) {
  pdf <- matrix(0, dataPoints, clu)
  
  for(j in 1:clu) {
    pdf[, j] <- gaussianProb(dataA, mu[j, ], sigma[[j]])
  }
  
  pd_w <- sweep(pdf, 2, phi, `*`)
  W <- sweep(pd_w, 1, rowSums(pd_w), `/`)
  
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

# Visualize clusters for now
cluster_labels <- apply(W, 1, which.max)
dataA <- as.data.frame(dataA)
dataA$cluster <- factor(cluster_labels)

ggplot(dataA, aes(x = V1, y = V2, color = cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "GMM Clustering on Autoencoder Features")
