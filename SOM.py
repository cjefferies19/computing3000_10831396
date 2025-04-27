import numpy as np
import pandas as pd
from minisom import MiniSom
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt

# Load the filled combined dataset
df = pd.read_csv('combined_weather_data.csv')

# Select the relevant features for clustering
X = df[['rainfall', 'temperature', 'wind']].values

# Normalise the data
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# SOM Parameters
som = MiniSom(x=10, y=10, input_len=X_scaled.shape[1], sigma=1.0, learning_rate=0.5)
som.train(X_scaled, 10000)

# Visualise SOM weights (heatmap)
weights = som.get_weights()

# Plotting weights for each feature (rainfall, temperature, wind)
plt.figure(figsize=(12, 8))

# Plot the weights of the first feature (rainfall)
plt.subplot(1, 3, 1)
plt.imshow(weights[:, :, 0], cmap='coolwarm', interpolation='nearest')
plt.title('SOM Weights - Rainfall')
plt.colorbar()

# Plot the weights of the second feature (temperature)
plt.subplot(1, 3, 2)
plt.imshow(weights[:, :, 1], cmap='coolwarm', interpolation='nearest')
plt.title('SOM Weights - Temperature')
plt.colorbar()

# Plot the weights of the third feature (wind)
plt.subplot(1, 3, 3)
plt.imshow(weights[:, :, 2], cmap='coolwarm', interpolation='nearest')
plt.title('SOM Weights - Wind')
plt.colorbar()

plt.tight_layout()
plt.show()

# Cluster assignments
som_clusters = np.array([som.winner(x) for x in X_scaled])
df['SOM_cluster'] = [x[0] * 10 + x[1] for x in som_clusters]

# Save SOM results to a CSV file
df.to_csv('som_clusters.csv', index=False)
print("SOM clustering completed and saved.")
