# Load the library
library(keras)

# Define the encoder
encoder <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(784)) %>%
  layer_dense(units = 2, activation = 'relu')

# Define the decoder
decoder <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(2)) %>%
  layer_dense(units = 784, activation = 'sigmoid')

# Connect them to create the autoencoder
autoencoder <- keras_model(inputs = encoder$input, outputs = decoder(encoder$output))

# Compile the model
autoencoder %>% compile(optimizer = 'adam', loss = 'binary_crossentropy')

# Load the data
data <- dataset_mnist()

# Prepare the data
x_train <- data$train$x
x_test <- data$test$x
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
x_train <- x_train / 255
x_test <- x_test / 255

# Train the model
autoencoder %>% fit(x_train, x_train, epochs = 50, batch_size = 256, validation_data = list(x_test, x_test))