# Libraries
#install.packages("psych")
library(psych)

# Work directory
setwd("/home/gerard/Desktop/MD/MD/")

# Read dataset
filename <- "dataset/filtered_data.csv"
dd <- read.csv(filename)
dd <- dd[, c("price_level", "vegan_options", "awards", "gluten_free", "cuisines", "original_location", "open_days_per_week", "avg_rating", "total_reviews_count", "food", "service", "atmosphere", "excellent", "meals")]

output_dirs <- c("bivariant/scatterplots", "bivariant/boxplots", "bivariant/histograms", "bivariant/barplots", "bivariant/mosaicplots")
for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

num_cols <- names(dd)[sapply(dd, is.numeric)]
cat_cols <- names(dd)[sapply(dd, function(col) is.character(col) || is.factor(col))]

# Iterate over all pairs of columns
column_pairs <- expand.grid(names(dd), names(dd), stringsAsFactors = FALSE)
column_pairs <- column_pairs[column_pairs[, 1] != column_pairs[, 2], ]

# Open a PDF file to store all the plots
pdf("bivariant_plots.pdf", width = 8, height = 6)

for (i in seq_len(nrow(column_pairs))) {
  col1 <- column_pairs[i, 1]
  col2 <- column_pairs[i, 2]
  
  if (col1 > col2) next
  
  # Scatterplots for numeric vs numeric
  if (col1 %in% num_cols && col2 %in% num_cols) {
    pairs.panels(dd[, c(col1, col2)], method = "pearson", hist.col = "lightblue", density = TRUE, ellipses = TRUE)
  }
  
  # Boxplots and histograms for numeric vs categorical (in both possible orders)
  if ((col1 %in% num_cols && col2 %in% cat_cols) || (col1 %in% cat_cols && col2 %in% num_cols)) {
    valid_data <- dd[!is.na(dd[[col1]]) & !is.na(dd[[col2]]), ]
    
    # Boxplot
    if (col1 %in% num_cols) {
      boxplot(valid_data[[col1]] ~ valid_data[[col2]], main = paste("Boxplot of", col1, "by", col2), xlab = col2, ylab = col1, col = "lightblue", border = "black")
    } else {
      boxplot(valid_data[[col2]] ~ valid_data[[col1]], main = paste("Boxplot of", col2, "by", col1), xlab = col1, ylab = col2, col = "lightblue", border = "black")
    }
    
    # Histogram
    if (col1 %in% num_cols) {
      hist(valid_data[[col1]], main = paste("Histogram of", col1, "grouped by", col2), xlab = col1, col = "lightblue", border = "black", breaks = 20)
    } else {
      hist(valid_data[[col2]], main = paste("Histogram of", col2, "grouped by", col1), xlab = col2, col = "lightblue", border = "black", breaks = 20)
    }
  }
  
  # Barplots and mosaic plots for categorical vs categorical
  if (col1 %in% cat_cols && col2 %in% cat_cols) {
    contingency_table <- table(dd[[col1]], dd[[col2]])
    
    # Barplot
    barplot(contingency_table, beside = TRUE, col = rainbow(nrow(contingency_table)), main = paste("Barplot of", col1, "vs", col2), xlab = col1, ylab = "Count", legend = TRUE)
    
    # Mosaicplot
    mosaicplot(contingency_table, main = paste("Mosaic Plot of", col1, "vs", col2), color = TRUE, shade = TRUE, las = 2)
  }
}

# Close the PDF device to save the file
dev.off()

print("Done")
