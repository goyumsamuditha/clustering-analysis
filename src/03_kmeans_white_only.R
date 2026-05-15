install.packages(c("factoextra", "cluster", "dendextend", "corrplot"))
# load libraries
library(readr)
library(dplyr)
library(cluster)
library(factoextra)

# load data
white_wine <- read_csv("data/processed/white_wine_data.csv")

# data preparing
X_white <- white_wine %>% select(1:11) 

#standardize the data
X_white_scaled <- scale(X_white)

# determine the optimal number of clusters using the elbow method
fviz_nbclust(X_white_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters"
       , x = "Number of Clusters"
       , y = "Total Within-Cluster Sum of Squares")

# determine the optimal number of clusters using the silhouette method
fviz_nbclust(X_white_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal Clusters"
       , x = "Number of Clusters"
       , y = "Average Silhouette Width")

# perform k-means clustering with the chosen number of clusters k=2 and k-3
set.seed(42)
kmeans_white_2 <- kmeans(X_white_scaled, centers = 2, nstart = 25)
kmeans_white_3 <- kmeans(X_white_scaled, centers = 3, nstart = 25)

# elbow method plot
png("output/figures/elbow_silhouette_kmeans_white.png", width = 1200, height = 600)
p1 <- fviz_nbclust(X_white_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters"
       , x = "Number of Clusters"
       , y = "Total Within-Cluster Sum of Squares")
p2 <- fviz_nbclust(X_white_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal Clusters"
       , x = "Number of Clusters"
       , y = "Average Silhouette Width")
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
dev.off()
message("Elbow and silhouette method plots saved to output/figures/elbow_silhouette_kmeans_white.png")

# validate the clusters using silhouette scores
silhouette_2 <- silhouette(kmeans_white_2$cluster, dist(X_white_scaled))
silhouette_3 <- silhouette(kmeans_white_3$cluster, dist(X_white_scaled))

cat("Average Silhouette Width for k=2: ", mean(silhouette_2[, 3]), "\n")
cat("Average Silhouette Width for k=3: ", mean(silhouette_3[, 3]), "\n")

# mean of each attribute for the wining clusters
white_wine$cluster_k2 <- kmeans_white_2$cluster
attribute_means <- white_wine %>%
    group_by(cluster_k2) %>%
    summarise(across(1:11, mean, .names = "mean_{col}"))

print("Mean of each attribute for the winning clusters (k=2):")
print(attribute_means)


# check consistency of clusters with quality
quality_check <- table(Cluster = white_wine$cluster_k2, Quality = white_wine$quality)
quality_proportions <- prop.table(quality_check, margin = 1)
print("Proportions of quality ratings within each cluster:")
print(quality_proportions)

# visualize the distribution of quality ratings within each cluster
png("output/figures/quality_distribution_k2.png", width = 800, height = 600)
boxplot(quality ~ cluster_k2, data = white_wine, 
main = "Quality Distribution by Cluster (k=2)", 
xlab = "Cluster", 
ylab = "Quality", 
col = c("gold", "white"))
dev.off()
message("K-means clustering completed and results saved to output/figures/quality_distribution_k2.png")