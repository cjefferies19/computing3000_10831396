# Load libraries
library(keras)
library(dplyr)
library(scales)

# Function to normalise data
normalize_data <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Load datasets and normalise them
rainfall <- read.csv("rainfall.csv") %>% lapply(normalize_data) %>% as.data.frame()
temperature <- read.csv("temperature.csv") %>% lapply(normalize_data) %>% as.data.frame()
wind <- read.csv("wind.csv") %>% lapply(normalize_data) %>% as.data.frame()
dem <- read.csv("dem.csv") %>% lapply(normalize_data) %>% as.data.frame()
land <- read.csv("land.csv") %>% lapply(normalize_data) %>% as.data.frame()
floodHistory <- read.csv("floodHistory.csv") %>% lapply(normalize_data) %>% as.data.frame()
population <- read.csv("population.csv") %>% lapply(normalize_data) %>% as.data.frame()

# Combine all datasets into a single matrix
big_data <- cbind(rainfall, temperature, wind, dem, land, floodHistory, population)
big_data <- as.matrix(big_data)

# Define input shape
input_shape <- ncol(big_data)

# Define the encoder
encoder <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = 'relu', input_shape = c(input_shape)) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'relu')  # 2D representation

# Define the decoder
decoder <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(2)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = input_shape, activation = 'sigmoid')

# Connect encoder and decoder
autoencoder <- keras_model(inputs = encoder$input, outputs = decoder(encoder$output))

# Compile the model
autoencoder %>% compile(optimizer = 'adam', loss = 'mean_squared_error')

# Train the model
autoencoder %>% fit(big_data, big_data, epochs = 100, batch_size = 32, validation_split = 0.2)

# Extract encoded features for unsupervised learning
encoded_data <- predict(encoder, big_data)

# Save data
write.csv(encoded_data, "newData.csv", row.names = FALSE)
