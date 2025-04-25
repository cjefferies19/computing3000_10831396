import numpy as np
import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
from keras.models import Model
from keras.layers import Input, Dense
from keras.optimizers import Adam

# Load your dataset
df = pd.read_csv('combined_weather_data.csv')

# Select relevant features
features = ['rainfall', 'temperature', 'wind']
X = df[features].values

# Normalize the data
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# Define the Autoencoder model
input_layer = Input(shape=(X_scaled.shape[1],))
encoded = Dense(8, activation='relu')(input_layer)  # Encoded representation (8-dimensional)
decoded = Dense(X_scaled.shape[1], activation='sigmoid')(encoded)  # Reconstructed input

# Autoencoder model
autoencoder = Model(input_layer, decoded)

# Encoder model (used to extract encoded representation)
encoder = Model(input_layer, encoded)

# Compile and train the autoencoder
autoencoder.compile(optimizer=Adam(), loss='mean_squared_error')
autoencoder.fit(X_scaled, X_scaled, epochs=50, batch_size=256, shuffle=True, validation_split=0.2)

# Get the encoded features
encoded_X = encoder.predict(X_scaled)

# Perform clustering on the encoded features
kmeans = KMeans(n_clusters=3, random_state=42)
kmeans_labels = kmeans.fit_predict(encoded_X)

# Add the clustering results to the dataframe
df['Autoencoder_cluster'] = kmeans_labels

# Save the results
df.to_csv('combined_with_autoencoder_clusters.csv', index=False)

# Visualize the clustering results (using the encoded features)
plt.scatter(encoded_X[:, 0], encoded_X[:, 1], c=kmeans_labels, cmap='viridis')
plt.title('Autoencoder Clustering Visualization')
plt.xlabel('Encoded Feature 1')
plt.ylabel('Encoded Feature 2')
plt.colorbar(label='Cluster')
plt.show()

# Show the first few rows of the final dataframe with clustering results
print(df.head())
