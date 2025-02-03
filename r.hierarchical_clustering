# Install and load necessary packages
install.packages("keras")
library(keras)

# Load the iris dataset
data(iris)

# Remove the species column for normalization
iris_data <- iris[, -5]

# Normalize the data to range [0, 1]
iris_data <- as.data.frame(scale(iris_data))

# Define the input layer
input_layer <- layer_input(shape = ncol(iris_data))

# Define the encoding layers
encoded <- input_layer %>%
  layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'relu')

# Define the decoding layers
decoded <- encoded %>%
  layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = ncol(iris_data), activation = 'sigmoid')

# Define the autoencoder model
autoencoder <- keras_model(input_layer, decoded)

# Compile the model
autoencoder %>% compile(optimizer = 'adam', loss = 'mean_squared_error')

# Train the autoencoder
history <- autoencoder %>% fit(
  as.matrix(iris_data),
  as.matrix(iris_data),
  epochs = 50,
  batch_size = 10,
  validation_split = 0.2
)

# Get the normalized data from the autoencoder
normalized_data <- autoencoder %>% predict(as.matrix(iris_data))

# Convert the normalized data back to a data frame
normalized_data <- as.data.frame(normalized_data)

# Add column names
colnames(normalized_data) <- colnames(iris_data)

# View the first few rows of the normalized dataset
head(normalized_data)
