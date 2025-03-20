# ==============================
# PRINCIPAL COMPONENT ANALYSIS SCRIPT
# ==============================
# This script performs Principal Component Analysis (PCA) on the numerical variables
# of the dataset, including visualization of variance explained, individuals,
# and factorial planes with both numerical and categorical variables.

# ==============================
# WORKING DIRECTORY & DATA LOADING
# ==============================

# Set working directory
setwd("/Users/miquelrodoreda/uni/MD")

# Load dataset
df <- read.csv("dataset/renamed.csv")

# Create output directory
directory <- "pca/"
if (!dir.exists(directory)) {
  dir.create(directory, recursive = TRUE)
}

# ==============================
# DATA PREPARATION
# ==============================

# Identify numerical and categorical variables
numericals <- which(sapply(df, is.numeric))
categoricals <- which(sapply(dd, function(col) is.character(col) || is.factor(col)))

# Extract numerical variables
df_numericals <- df[, numericals]
numericals_names <- names(df_numericals)

# Display variables used for PCA
print(paste("The PCA analysis is going to be done for these numerical variables:", 
            paste(numericals_names, collapse = ", ")))

# ==============================
# PCA ANALYSIS
# ==============================

# Perform PCA
principal_components <- prcomp(df_numericals, scale = TRUE)

# Display PCA results
print("PCA results:")
print(principal_components$rotation)

# Calculate variance explained
variance <- (principal_components$sdev)^2
variance_ratio <- variance / sum(variance)
cumulative_variance <- cumsum(variance_ratio)

# ==============================
# VARIANCE VISUALIZATION
# ==============================

# Plot variance explained by each component
png(paste0(directory, "variance.png"), width = 800, height = 600)
barplot(100 * variance_ratio, 
        main = "Variance Explained by Principal Components",
        xlab = "Principal Components", 
        ylab = "Variance Explained (%)", 
        col = "lightblue")
dev.off()

# Plot cumulative variance
png(paste0(directory, "cumulative_variance.png"), width = 800, height = 600)
barplot(100 * cumulative_variance, 
        main = "Cumulative Variance Explained",
        xlab = "Principal Components", 
        ylab = "Cumulative Variance (%)", 
        col = "lightgreen")
dev.off()

# ==============================
# INDIVIDUALS VISUALIZATION
# ==============================

# Select dimensions for visualization (first two components)
nd <- 2
Psi <- principal_components$x[, 1:nd]
dim1 <- 1
dim2 <- 2
iden <- row.names(df_numericals)

# Plot individuals
png(paste0(directory, "individuals_graphic.png"), width = 800, height = 600)
plot(Psi[, dim1], Psi[, dim2], 
     main = "Individuals Graphic", 
     xlab = "PC1", 
     ylab = "PC2")
axis(side = 1, pos = 0, labels = FALSE, col = "cyan")
axis(side = 3, pos = 0, labels = FALSE, col = "cyan")
axis(side = 2, pos = 0, labels = FALSE, col = "cyan")
axis(side = 4, pos = 0, labels = FALSE, col = "cyan")
dev.off()

# ==============================
# FACTORIAL PLANES
# ==============================

# Calculate correlations between variables and principal components
Phi <- cor(df_numericals, Psi)
X <- Phi[, dim1]
Y <- Phi[, dim2]
ze <- rep(0, length(numericals_names))

# Plot factorial plane with numerical variables
png(paste0(directory, "factorial_plane_only_numericals.png"), width = 800, height = 600)
plot(Psi[, dim1], Psi[, dim2], 
     type = "n", 
     xlim = c(min(X, 0), max(X, 0)), 
     ylim = c(-1, 1))
axis(side = 1, pos = 0, labels = FALSE)
axis(side = 3, pos = 0, labels = FALSE)
axis(side = 2, pos = 0, labels = FALSE)
axis(side = 4, pos = 0, labels = FALSE)
arrows(ze, ze, X, Y, length = 0.07, col = "blue")
text(X, Y, labels = etiq, col = "darkblue", cex = 0.7)
dev.off()

# Plot factorial plane with both numerical and categorical variables
colors <- rainbow(length(categoricals))
png(paste0(directory, "factorial_plane_numericals_with_categoricals.png"), width = 800, height = 600)
plot(Psi[, dim1], Psi[, dim2], 
     type = "n", 
     xlim = c(min(X, 0), max(X, 0)), 
     ylim = c(-2, 1))
axis(side = 1, pos = 0, labels = FALSE, col = "cyan")
axis(side = 3, pos = 0, labels = FALSE, col = "cyan")
axis(side = 2, pos = 0, labels = FALSE, col = "cyan")
axis(side = 4, pos = 0, labels = FALSE, col = "cyan")
arrows(ze, ze, X, Y, length = 0.07, col = "lightgray")
text(X, Y, labels = etiq, col = "gray", cex = 0.7)

# Add categorical variables
c <- 1
for (k in categoricals) {
  nextColor <- colors[c]
  fdic1 <- tapply(Psi[, dim1], df[, k], mean)
  fdic2 <- tapply(Psi[, dim2], df[, k], mean)
  
  text(fdic1, fdic2, 
       labels = levels(factor(df[, k])), 
       col = nextColor, 
       cex = 0.6)
  c <- c + 1
}
legend("bottomleft", 
       names(df)[categoricals], 
       pch = 1, 
       col = colors, 
       cex = 0.6)
dev.off()

# ==============================
# GROUPED FACTORIAL PLANES
# ==============================

# Function to generate factorial planes for groups of categorical variables
generate_factorial_plane <- function(group_num, categories_names, xlim_vals, ylim_vals) {
  group_categories <- which(names(df) %in% categories_names)
  colors <- rainbow(length(group_categories))
  
  png(paste0(directory, "factorial_plane_group_", group_num, ".png"), 
      width = 800, height = 600)
  
  plot(Psi[, dim1], Psi[, dim2], 
       type = "n", 
       xlim = xlim_vals, 
       ylim = ylim_vals)
  
  # Add axes
  axis(side = 1, pos = 0, labels = FALSE, col = "cyan")
  axis(side = 3, pos = 0, labels = FALSE, col = "cyan")
  axis(side = 2, pos = 0, labels = FALSE, col = "cyan")
  axis(side = 4, pos = 0, labels = FALSE, col = "cyan")
  
  # Add numerical variables
  arrows(ze, ze, X, Y, length = 0.07, col = "lightgray")
  text(X, Y, labels = etiq, col = "gray", cex = 0.7)
  
  # Add categorical variables
  c <- 1
  for (k in group_categories) {
    seguentColor <- colors[c]
    fdic1 <- tapply(Psi[, dim1], df[, k], mean)
    fdic2 <- tapply(Psi[, dim2], df[, k], mean)
    
    text(fdic1, fdic2, 
         labels = levels(factor(df[, k])), 
         col = seguentColor, 
         cex = 0.6)
    c <- c + 1
  }
  
  # Add legend
  legend("bottomleft", 
         names(df)[group_categories], 
         pch = 1, 
         col = colors, 
         cex = 0.6)
  
  dev.off()
}

# Generate factorial planes for different groups of categorical variables
# Group 1: Price and dietary options
categories_names <- c("price_level", "vegan_options", "gluten_free")
generate_factorial_plane(1, categories_names, c(-1, 1.5), c(-2, 1))

# Group 2: Location and cuisine types
categories_names <- c("original_location", "cuisines")
generate_factorial_plane(2, categories_names, c(-1, 1.5), c(-2, 1))

# Group 3: Meals and awards
categories_names <- c("meals", "awards")
generate_factorial_plane(3, categories_names, c(-3.5, 1.5), c(-2, 1))
