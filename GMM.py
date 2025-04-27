import numpy as np
import pandas as pd
from sklearn.mixture import GaussianMixture
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler

# Load the filled combined dataset
df = pd.read_csv('combined_weather_data.csv')

# Select the relevant features for clustering
X = df[['rainfall', 'temperature', 'wind']].values

# Normalise the data
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# Fit the model
gmm = GaussianMixture(n_components=3)  # Number of clusters given
gmm.fit(X_scaled)

# Predict the cluster labels
df['GMM_cluster'] = gmm.predict(X_scaled)

# Visualise GMM clustering result in 2D
plt.figure(figsize=(10, 6))
plt.scatter(df['rainfall'], df['temperature'], c=df['GMM_cluster'], cmap='viridis', marker='o')
plt.title("GMM Clustering: Rainfall vs Temperature")
plt.xlabel('Rainfall')
plt.ylabel('Temperature')
plt.colorbar(label='Cluster')
plt.show()

# Visualise GMM clustering result in 2D
plt.figure(figsize=(10, 6))
plt.scatter(df['temperature'], df['wind'], c=df['GMM_cluster'], cmap='viridis', marker='o')
plt.title("GMM Clustering: Temperature vs Wind")
plt.xlabel('Temperature')
plt.ylabel('Wind')
plt.colorbar(label='Cluster')
plt.show()

# Save the GMM clustering results to a CSV file
df.to_csv('gmm_clusters.csv', index=False)
print("GMM clustering completed and saved.")
