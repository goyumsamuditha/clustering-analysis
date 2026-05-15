# load libraries
library(dendextend)
library(corrplot)

# load data
white_wine <- read_csv("data/processed/white_wine_data.csv")

# sampling for readable dendrogram
set.seed(123)
sample_indices <- white_wine %>% sample_n(150)
X_sample_scaled <- scale(sample_indices %>% select(1:11))

# compute distance matrix
dist_matrix <- dist(X_sample_scaled, method = "euclidean")

# perform hierarchical clustering
hclust_single <- hclust(dist_matrix, method = "single")
hclust_complete <- hclust(dist_matrix, method = "complete")
hclust_average <- hclust(dist_matrix, method = "average")

# visualize dendrograms
png("output/figures/dendrograms.png", width = 1400, height = 600)
par(mfrow = c(1, 3))
plot(hclust_single, main = "Hierarchical Clustering (Single Linkage)", labels = FALSE, hang = -1)
plot(hclust_complete, main = "Hierarchical Clustering (Complete Linkage)", labels = FALSE, hang = -1)   
plot(hclust_average, main = "Hierarchical Clustering (Average Linkage)", labels = FALSE, hang = -1)
dev.off()

message("Hierarchical clustering completed and dendrograms saved to output/figures/dendrograms.png")

# compute correlation matrix and visualize it
cor_matrix <- cor(X_sample_scaled)
png("output/figures/correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
dev.off()

message("Correlation matrix computed and saved to output/figures/correlation_matrix.png")

# correlation comparison
cor_single <- cor(dist_matrix, cophenetic(hclust_single))
cor_complete <- cor(dist_matrix, cophenetic(hclust_complete))
cor_average <- cor(dist_matrix, cophenetic(hclust_average))

message("Cophenetic Correlation for Single Linkage: \n")
cat("Single:   ", cor_single, "\n")
cat("Complete: ", cor_complete, "\n")
cat("Average:  ", cor_average, "\n")

# simmilarity comparison
dend_single <- as.dendrogram(hclust_single)
dend_complete <- as.dendrogram(hclust_complete)
dend_average <- as.dendrogram(hclust_average)

dend_list <- dendlist(Single = dend_single, Complete = dend_complete, Average = dend_average)

dend_cor_matrix <- cor.dendlist(dend_list, method = "cophenetic")

# generate corplot
png("output/figures/dendrogram_correlation.png", width = 800, height = 800)
corrplot(dend_cor_matrix, method = "pie", type = "upper", 
            title = "Similarity of Clustering Methods", 
            mar = c(0,0,2,0))
dev.off()
message("Dendrogram correlation matrix computed and saved to output/figures/dendrogram_correlation.png")
