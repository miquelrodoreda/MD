# ==============================
# BIVARIATE ANALYSIS PDF GENERATION SCRIPT
# ==============================
# This script generates a PDF containing all bivariate analysis plots,
# including scatterplots, boxplots, histograms, barplots, and mosaic plots
# for all pairs of variables in the dataset.

# ==============================
# LIBRARIES
# ==============================

# Load required libraries
library(psych)

# ==============================
# WORKING DIRECTORY & DATA LOADING
# ==============================

# Set working directory
setwd("/home/gerard/Desktop/MD/MD/")

# Load dataset
filename <- "dataset/filtered_data.csv"
dd <- read.csv(filename)

# Select relevant columns
dd <- dd[, c("price_level", "vegan_options", "awards", "gluten_free", 
             "cuisines", "original_location", "open_days_per_week", 
             "avg_rating", "total_reviews_count", "food", "service", 
             "atmosphere", "excellent", "meals")]

# ==============================
# OUTPUT DIRECTORY SETUP
# ==============================

# Define output directories
output_dirs <- c("bivariant/scatterplots", 
                 "bivariant/boxplots", 
                 "bivariant/histograms", 
                 "bivariant/barplots", 
                 "bivariant/mosaicplots")

# Create directories if they don't exist
for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# ==============================
# VARIABLE IDENTIFICATION
# ==============================

# Identify numeric and categorical variables
num_cols <- names(dd)[sapply(dd, is.numeric)]
cat_cols <- names(dd)[sapply(dd, function(col) is.character(col) || is.factor(col))]

# ==============================
# PAIRWISE COLUMN COMBINATIONS
# ==============================

# Generate all unique column pairs (excluding self-pairing)
column_pairs <- expand.grid(names(dd), names(dd), stringsAsFactors = FALSE)
column_pairs <- column_pairs[column_pairs[, 1] != column_pairs[, 2], ]

# ==============================
# PDF GENERATION
# ==============================

# Open a PDF file to store all the plots
pdf("bivariant_plots.pdf", width = 8, height = 6)

# Iterate over column pairs to generate plots
for (i in seq_len(nrow(column_pairs))) {
  col1 <- column_pairs[i, 1]
  col2 <- column_pairs[i, 2]
  
  # Skip duplicate combinations
  if (col1 > col2) next
  
  # ==============================
  # NUMERIC VS NUMERIC PLOTS
  # ==============================
  
  if (col1 %in% num_cols && col2 %in% num_cols) {
    pairs.panels(dd[, c(col1, col2)], 
                method = "pearson", 
                hist.col = "lightblue", 
                density = TRUE, 
                ellipses = TRUE)
  }
  
  # ==============================
  # NUMERIC VS CATEGORICAL PLOTS
  # ==============================
  
  if ((col1 %in% num_cols && col2 %in% cat_cols) || 
      (col1 %in% cat_cols && col2 %in% num_cols)) {
    # Remove rows with missing values
    valid_data <- dd[!is.na(dd[[col1]]) & !is.na(dd[[col2]]), ]
    
    # Generate boxplot
    if (col1 %in% num_cols) {
      boxplot(valid_data[[col1]] ~ valid_data[[col2]], 
              main = paste("Boxplot of", col1, "by", col2), 
              xlab = col2, 
              ylab = col1, 
              col = "lightblue", 
              border = "black")
    } else {
      boxplot(valid_data[[col2]] ~ valid_data[[col1]], 
              main = paste("Boxplot of", col2, "by", col1), 
              xlab = col1, 
              ylab = col2, 
              col = "lightblue", 
              border = "black")
    }
    
    # Generate histogram
    if (col1 %in% num_cols) {
      hist(valid_data[[col1]], 
           main = paste("Histogram of", col1, "grouped by", col2), 
           xlab = col1, 
           col = "lightblue", 
           border = "black", 
           breaks = 20)
    } else {
      hist(valid_data[[col2]], 
           main = paste("Histogram of", col2, "grouped by", col1), 
           xlab = col2, 
           col = "lightblue", 
           border = "black", 
           breaks = 20)
    }
  }
  
  # ==============================
  # CATEGORICAL VS CATEGORICAL PLOTS
  # ==============================
  
  if (col1 %in% cat_cols && col2 %in% cat_cols) {
    # Create contingency table
    contingency_table <- table(dd[[col1]], dd[[col2]])
    
    # Generate barplot
    barplot(contingency_table, 
            beside = TRUE, 
            col = rainbow(nrow(contingency_table)), 
            main = paste("Barplot of", col1, "vs", col2), 
            xlab = col1, 
            ylab = "Count", 
            legend = TRUE)
    
    # Generate mosaic plot
    mosaicplot(contingency_table, 
               main = paste("Mosaic Plot of", col1, "vs", col2), 
               color = TRUE, 
               shade = TRUE, 
               las = 2)
  }
}

# Close the PDF device to save the file
dev.off()

print("Done")
