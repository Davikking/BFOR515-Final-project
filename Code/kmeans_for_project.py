import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
#=========================================================================================
# this section handles the kmeans clustering note that kmeans using the elbow method and
# 10 iterations creates an incredibly large data set of roughly 7 million datapoints. 
# RUN ON A SYSTEM WITH LARGE RAM STORAGE OR CHANGE RANGE FROM 10 TO 5
#=========================================================================================
#import the data from
df = pd.read_csv(r"C:\Users\Christoph\Documents\scaled_data.csv")

# Select relevant columns for clustering
data = df[['TOTAL...CRITICAL.VIOLATIONS', 'SCORE', 'GRADE']]

# Handle missing data (if any)
data = data.dropna()

# Scale the data
scaler = StandardScaler()
scaled_data = scaler.fit_transform(data)

# Elbow Method to determine optimal k
wss = []
for k in range(1, 11):
    kmeans = KMeans(n_clusters=k, n_init=25)  # n_jobs has been removed
    kmeans.fit(scaled_data)
    wss.append(kmeans.inertia_)

# Plot the Elbow graph
plt.plot(range(1, 11), wss, marker='o')
plt.title('Elbow Method for Optimal Clusters')
plt.xlabel('Number of Clusters')
plt.ylabel('Within Sum of Squares (WSS)')
plt.show()

# Fit the model with optimal k (say 3)
optimal_k = 4
kmeans = KMeans(n_clusters=optimal_k, n_init=25)
df['Cluster'] = kmeans.fit_predict(scaled_data)
# Plot the clusters in 3D space
fig = plt.figure(figsize=(10, 8))
ax = fig.add_subplot(111, projection='3d')

# Scatter plot: x, y, and z represent the 3 features
ax.scatter(df['TOTAL...CRITICAL.VIOLATIONS'], df['SCORE'], df['GRADE'], 
           c=df['Cluster'], cmap='viridis', s=50)

# Add labels and title
ax.set_xlabel('Total Critical Violations')
ax.set_ylabel('Score')
ax.set_zlabel('Grade')
ax.set_title('KMeans Clustering of Restaurant Inspections')

plt.show()

# Save the clustered data to CSV
df.to_csv('clustered_data.csv', index=False)
# Save the clustered data
#df.to_csv('clustered_data.csv', index=False)