# ==============================
# CLUSTERING ANALYSIS SCRIPT
# ==============================
# This script performs clustering analysis on the dataset using both K-means and
# hierarchical clustering methods. It includes visualization of clusters and
# analysis of clustering quality.

# ==============================
# LIBRARIES
# ==============================

# Load required libraries
library(cluster)

# ==============================
# WORKING DIRECTORY & DATA LOADING
# ==============================

# Set working directory
setwd("/Users/miquelrodoreda/uni/MD")

# Load dataset
dd <- read.csv("dataset/renamed.csv")

# Display basic dataset information
names(dd)
dim(dd)
summary(dd)

# ==============================
# DATA PREPARATION
# ==============================

# Get the names of numeric columns
num_cols <- names(dd)[sapply(dd, is.numeric)]

# Filter only numeric columns
dcon <- dd[, num_cols, drop = FALSE]  # drop=FALSE prevents errors when selecting a single column

# Verify that dcon contains data
print(dim(dcon))

# Display the variables used for clustering
print(paste("The clustering is going to be done for these numerical variables:", 
            paste(names(dcon), collapse = ", ")))

# ==============================
# K-MEANS CLUSTERING
# ==============================

# Initial K-means run with k=5
k1 <- kmeans(dcon, 5)
names(dcon)
print(k1)

# Display K-means attributes
attributes(k1)
k1$size
k1$withinss
k1$centers

# Compute the decomposition of inertia
Bss <- sum(rowSums(k1$centers^2) * k1$size)
Wss <- sum(k1$withinss)
Tss <- k1$totss

# Verify total inertia
Bss + Wss

# Calculate between-cluster inertia percentage
Ib1 <- 100 * Bss / (Bss + Wss)
Ib1

# Repeat K-means run with k=5 for comparison
k2 <- kmeans(dcon, 5)
k2$size

# Compute inertia for second run
Bss <- sum(rowSums(k2$centers^2) * k2$size)
Wss <- sum(k2$withinss)
Ib2 <- 100 * Bss / (Bss + Wss)

# Compare inertia between runs
Ib2
Ib1

# Compare cluster centers
k2$centers
k1$centers

# ==============================
# VISUALIZATION
# ==============================

# Create output directory if it doesn't exist
if (!dir.exists("clustering")) {
  dir.create("clustering")
}

# Plot cluster centers
png("clustering/kmeans_centers.png", width = 800, height = 600)
plot(k1$centers[, 3], k1$centers[, 2], 
     main = "KMeans: Centers of Clusters")
dev.off()

# Compare clusters between runs
table(k1$cluster, k2$cluster)

# Hierarchical clustering
png("clustering/hierarchical_clustering.png", width = 800, height = 600)
d <- dist(dcon)
h1 <- hclust(d, method = "ward.D2")
plot(h1, main = "Hierarchical Clustering")
dev.off()

# Cut the dendrogram at different levels
nc <- 3
c1 <- cutree(h1, nc)

nc <- 5
c5 <- cutree(h1, nc)

# Partition plot by classes
png("clustering/clustering_partition.png", width = 800, height = 600)
plot(dcon$total_reviews_count, dcon$avg_rating, 
     col = c1, 
     main = "Clustering: total_reviews_count vs avg_rating")
legend("topright", 
       c("class1", "class2", "class3"), 
       pch = 1, 
       col = c(1:3), 
       cex = 0.6)
dev.off()

# Boxplot of variables
png("clustering/boxplot_avg_rating.png", width = 800, height = 600)
boxplot(dd[, 7] ~ c2, 
        horizontal = TRUE, 
        main = "Boxplot: avg_rating by Clusters")
dev.off()

# 2D clustering plot
png("clustering/clustering_2D.png", width = 800, height = 600)
pairs(dcon[, 1:7], 
      col = c1, 
      main = "Clustering: Pairwise Plot")
dev.off()

# Quality of hierarchical partitioning
Bss <- sum(rowSums(cdg^2) * as.numeric(table(c1)))
Ib4 <- 100 * Bss / Tss
Ib4

# ==============================
# GOWER DISTANCE CLUSTERING
# ==============================

# Prepare data for Gower distance
actives <- c(1:14)
dd[, sapply(dd, is.character)] <- lapply(dd[, sapply(dd, is.character)], as.factor)
str(dd)

# Compute dissimilarity matrix
dissimMatrix <- daisy(dd[, actives], metric = "gower", stand = TRUE)
distMatrix <- dissimMatrix^2

# Perform hierarchical clustering with Gower distance
h1 <- hclust(distMatrix, method = "ward.D")
png("clustering/gower_clustering.png", width = 800, height = 600)
plot(h1, main = "Gower Clustering")
dev.off()

# Cut dendrogram into 4 clusters
c2 <- cutree(h1, 4)

# ==============================
# CLUSTER VISUALIZATION
# ==============================

# Visualization of clusters
png("clustering/comparison_clusters.png", width = 800, height = 600)
plot(avg_rating, total_reviews_count, 
     col = c2, 
     main = "Clustering of data in 3 classes")
legend("topright", 
       legend = unique(c2), 
       pch = 1, 
       col = c(1:4), 
       cex = 0.6)
dev.off()

# Boxplot for open days
png("clustering/boxplot_open_days.png", width = 800, height = 600)
boxplot(dd[, 6] ~ c2, 
        horizontal = TRUE, 
        main = "Boxplot: open_days_per_week by Clusters")
dev.off()

# Boxplot for total reviews
png("clustering/boxplot_total_reviews.png", width = 800, height = 600)
boxplot(dd[, 9] ~ c2, 
        horizontal = TRUE, 
        main = "Boxplot: total_reviews_count by Clusters")
dev.off()

# Comparison of partitions
table(c1, c2)

# Visualization of profiles
cdg <- aggregate(as.data.frame(dcon), list(c2), mean)
png("clustering/clustering_profile.png", width = 800, height = 600)
plot(avg_rating, total_reviews_count, col = c2)
points(cdg[, 4], cdg[, 5], pch = 16, col = "orange")
text(cdg[, 4], cdg[, 5], 
     labels = cdg[, 1], 
     pos = 2, 
     font = 2, 
     cex = 0.7, 
     col = "orange")
dev.off()

# Plot relationships between variables
potencials <- c("open_days_per_week", "avg_rating", "food", "service")
png("clustering/pairs_plot.png", width = 800, height = 600)
pairs(dcon[, potencials], col = c2)
dev.off()
