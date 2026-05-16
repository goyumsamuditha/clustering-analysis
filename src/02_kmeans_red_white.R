library(caret)
library(factoextra)
library(dplyr)
library(readr)
library(ggplot2)

# load data
wine_data <- read_csv("data/processed/wine_data.csv")

# data preparing
X_scaled <- scale(wine_data %>% select(1:11))

# determine the optimal number of clusters using the elbow method
png("output/figures/elbow_method.png", width = 800, height = 600)
fviz_nbclust(X_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters"
       , x = "Number of Clusters"
       , y = "Total Within-Cluster Sum of Squares")
dev.off()
message("Elbow method plot saved to output/figures/elbow_method.png")

#k-means
set.seed(42)
kmeans_result <- kmeans(X_scaled, centers = 2, nstart = 25)
wine_data$cluster <- kmeans_result$cluster

map_table <- table(wine_data$wine_type, wine_data$cluster)
white_cluster_id <- which.max(map_table["white", ])
wine_data$predicted_type <- ifelse(wine_data$cluster == white_cluster_id, "white", "red")

# PCA for visualization
png("output/figures/kmeans_clusters.png", width = 800, height = 600)
print(fviz_cluster(kmeans_result, data = X_scaled, geom = "point", ellipse.type = "convex") +
  labs(title = "PCA: Chemical Separation of Red vs. White Wine"))
dev.off()
message("K-means clustering plot saved to output/figures/kmeans_clusters.png")

#confusion matrix
conf_matrix <- confusionMatrix(as.factor(wine_data$predicted_type), as.factor(wine_data$wine_type))
plt_conf_matrix <- as.data.frame(conf_matrix$table)
png("output/figures/confusion_matrix.png", width = 800, height = 600)
print(ggplot(plt_conf_matrix, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 8, color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Confusion Matrix: K-means Clustering of Wine Types"))
dev.off()
message("Confusion matrix plot saved to output/figures/confusion_matrix.png")
print(conf_matrix$table)
print(conf_matrix)