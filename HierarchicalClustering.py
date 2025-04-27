import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.cluster import AgglomerativeClustering
from sklearn.neighbors import NearestNeighbors
from scipy.stats import mode

# Load and preprocess the full dataset
df = pd.read_csv('combined_weather_data.csv')
features = ['rainfall', 'temperature', 'wind']

# Standardise
X = df[features].values
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# PCA to 2D (for speed & consistency)
pca = PCA(n_components=2, random_state=42)
X_pca = pca.fit_transform(X_scaled)

# Prepare ensemble parameters
n_subsets = 5 # number of small hierarchical clusterings
subset_size = 1000 # size of each subset
n_clusters = 3 # desired number of clusters

# Storage for labels from each subset
labels_matrix = np.zeros((len(df), n_subsets), dtype=int)

for i in range(n_subsets):
    # Random subset indices
    idx = np.random.choice(len(df), subset_size, replace=False)
    X_sub = X_pca[idx]

    # Hierarchical clustering on this subset
    agg = AgglomerativeClustering(n_clusters=n_clusters, linkage='ward')
    sub_labels = agg.fit_predict(X_sub)

    # Build a 1-NN model on subset in PCA space
    nn = NearestNeighbors(n_neighbors=1).fit(X_sub)

    # For every full-data point, find nearest neighbor in subset
    _, nbr_idx = nn.kneighbors(X_pca)
    labels_matrix[:, i] = sub_labels[nbr_idx.flatten()]

# Majority vote across subset clusterings
final_labels = mode(labels_matrix, axis=1).mode.flatten()
df['Hierarchical_cluster'] = final_labels

# Save and visualise
df.to_csv('hierarchical_clustered_ensemble.csv', index=False)
print("✅ Ensemble hierarchical clustering complete. Saved to 'hierarchical_clustered_ensemble.csv'.")

# 2D plot in PCA space
import matplotlib.pyplot as plt
plt.figure(figsize=(7,5))
scatter = plt.scatter(
    X_pca[:,0], X_pca[:,1],
    c=final_labels, cmap='tab10', s=15, alpha=0.7
)
plt.title('Ensemble Hierarchical Clustering (PCA-reduced)')
plt.xlabel('PC 1')
plt.ylabel('PC 2')
plt.legend(*scatter.legend_elements(), title="Cluster")
plt.show()
