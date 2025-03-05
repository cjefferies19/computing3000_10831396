# Install and load necessary packages
install.packages("keras")
library(keras)

# Load data from CSV file
new_data <- read.csv("newData.csv")

# Ensure the data is numeric
numeric_data <- new_data[sapply(new_data, is.numeric)]

# Normalise the data to range [0, 1]
numeric_data <- as.data.frame(scale(numeric_data))

# Define the input layer
input_layer <- layer_input(shape = ncol(numeric_data))

# Define the encoding layers
encoded <- input_layer %>%
  layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'relu')

# Define the decoding layers
decoded <- encoded %>%
  layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = ncol(numeric_data), activation = 'sigmoid')

# Define the autoencoder model
autoencoder <- keras_model(input_layer, decoded)

# Compile the model
autoencoder %>% compile(optimizer = 'adam', loss = 'mean_squared_error')

# Train the autoencoder
history <- autoencoder %>% fit(
  as.matrix(numeric_data),
  as.matrix(numeric_data),
  epochs = 50,
  batch_size = 10,
  validation_split = 0.2
)

# Get the normalised data from the autoencoder
normalized_data <- autoencoder %>% predict(as.matrix(numeric_data))

# Convert the normalised data back to a data frame
normalized_data <- as.data.frame(normalized_data)

# Add column names
colnames(normalized_data) <- colnames(numeric_data)

# View the first few rows of the normalized dataset
head(normalized_data)

