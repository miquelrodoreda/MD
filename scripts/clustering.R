# ==============================
# CLUSTERING ANALYSIS SCRIPT
# ==============================
# This script performs clustering analysis on the dataset to identify
# natural groupings of restaurants based on their characteristics.

# ==============================
# LIBRARIES
# ==============================

# Load required libraries
library(cluster)
library(ggplot2)
library(pheatmap)

# ==============================
# WORKING DIRECTORY & DATA LOADING
# ==============================

# Set working directory
setwd("/home/gerard/Desktop/MD/MD/")

# Load dataset
dd <- read.csv("dataset/renamed.csv")
df <- read.csv("dataset/renamed.csv")

# ==============================
# DATA PREPARATION
# ==============================

# Identify numerical variables
numericals <- which(sapply(df, is.numeric))

# Identify categorical variables (character or factor)
categoricals <- which(sapply(df, function(col) is.character(col) || is.factor(col)))

# Subset numerical and categorical columns
df_numericals <- df[, numericals]
df_categoricals <- df[, categoricals]

# Convert categorical columns to factors
df[, categoricals] <- lapply(df[, categoricals], factor)

# Get the names of numerical and categorical variables
numericals_names <- names(df_numericals)
categoricals_names <- names(df_categoricals)

# ==============================
# CLUSTERING
# ==============================

# Create output directory if it doesn't exist
if (!dir.exists("clustering")) {
  dir.create("clustering")
}

# Hierarchical clustering
d <- daisy(df, metric = "gower")
h1 <- hclust(as.dist(d), method = "ward.D2")
png("clustering/hierarchical_clustering.png", width = 800, height = 600)
plot(h1, main = "Hierarchical Clustering", labels = FALSE, hang = -1)
dev.off()

heights <- c(
  k3 = h1$height[length(h1$height) - (3 - 1)],
  k4 = h1$height[length(h1$height) - (4 - 1)],
  k5 = h1$height[length(h1$height) - (5 - 1)]
)

nc <- 3
c3 <- cutree(h1, nc)

cluster_counts <- table(c3)
print(cluster_counts)

# 2D clustering plot
png("clustering/clustering_2D.png", width = 800, height = 600)
pairs(df_numericals, 
      col = c3, 
      main = "Clustering: Pairwise Plot")
dev.off()

for (var in numericals_names) {
  png(paste0("clustering/boxplot_", var, ".png"), width = 800, height = 600)
  
  # Create boxplot for each variable by clusters
  boxplot(df[[var]] ~ c3, 
          horizontal = TRUE, 
          main = paste("Boxplot: ", var, " by Clusters"))
  
  dev.off()
}

for (var in categoricals_names) {
  png(paste0("clustering/barplot_", var, ".png"), width = 800, height = 600)
  
  # Create bar plot for each categorical variable by clusters
  p <- ggplot(df, aes_string(x = var, fill = as.factor(c3))) +
    geom_bar(position = "dodge") +
    labs(title = paste("Bar Plot: ", var, " by Clusters"), x = var, y = "Frequency", fill = "Cluster") +
    theme_minimal()
  
  # Explicitly print the plot
  print(p)
  
  dev.off()
}
