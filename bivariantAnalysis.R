# Libraries
#install.packages("psych")
library(psych)
library(RColorBrewer)

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

for (i in seq_len(nrow(column_pairs))) {
  col1 <- column_pairs[i, 1]
  col2 <- column_pairs[i, 2]
  
  if (col1 > col2) next
  
  # Scatterplots for numeric vs numeric
  if (col1 %in% num_cols && col2 %in% num_cols) {
    file_name <- paste0("bivariant/scatterplots/scatterplot_", col1, "_", col2, ".png")
    png(file_name, width = 800, height = 600)
    pairs.panels(dd[, c(col1, col2)], method = "pearson", hist.col = "lightblue", density = TRUE, ellipses = TRUE)
    dev.off()
  }
  
  # Boxplots and histograms for numeric vs categorical (in both possible orders)
  if ((col1 %in% num_cols && col2 %in% cat_cols) || (col1 %in% cat_cols && col2 %in% num_cols)) {
    if (col1 %in% num_cols) {
      numeric_col <- col1
      categorical_col <- col2
    } else {
      numeric_col <- col2
      categorical_col <- col2
    }
    valid_data <- dd[!is.na(dd[[col1]]) & !is.na(dd[[col2]]), ]

    # Boxplot
    boxplot_file <- paste0("bivariant/boxplots/boxplot_", col1, "_", col2, ".png")
    png(boxplot_file, width = 800, height = 600)
    if (col1 %in% num_cols) {
      boxplot(valid_data[[col1]] ~ valid_data[[col2]], main = paste("Boxplot of", col1, "by", col2), xlab = col2, ylab = col1, col = "lightblue", border = "black")
    } else {
      boxplot(valid_data[[col2]] ~ valid_data[[col1]], main = paste("Boxplot of", col2, "by", col1), xlab = col1, ylab = col2, col = "lightblue", border = "black")
    }
    dev.off()
    
    # Histogram
    hist_file <- paste0("bivariant/histograms/histogram_", col1, "_", col2, ".png")
    png(hist_file, width = 800, height = 600)
    
    # Get the unique categories in col2
    categories <- unique(valid_data[[categorical_col]])
    category_colors <- brewer.pal(length(categories), "Set3")
    
    # Plot the first histogram
    hist(valid_data[[numeric_col]][valid_data[[categorical_col]] == categories[1]], 
         main = paste("Overlayed Histogram of", numeric_col, "grouped by", categorical_col),
         xlab = numeric_col, col = rgb(0, 0, 1, 0.5), border = "black", 
         breaks = 20, xlim = range(valid_data[[numeric_col]]), probability = TRUE, 
         freq = FALSE)
    
    # Overlay histograms for each category, ensuring the same 'breaks' and transparent colors
    for (i in seq_along(categories[-1])) {
      hist(valid_data[[numeric_col]][valid_data[[categorical_col]] == categories[i+1]], 
           col = category_colors[i], border = "black", 
           breaks = 20, add = TRUE, probability = TRUE, freq = FALSE)
    }
    
    # Add a legend
    legend("topright", legend = categories, fill = col2rgb(category_colors))
    
    dev.off()
  }
  
  # Barplots and mosaic plots for categorical vs categorical
  if (col1 %in% cat_cols && col2 %in% cat_cols) {
    contingency_table <- table(dd[[col1]], dd[[col2]])
    
    # Barplot
    barplot_file <- paste0("bivariant/barplots/barplot_", col1, "_", col2, ".png")
    png(barplot_file, width = 800, height = 600)
    barplot(contingency_table, beside = TRUE, col = rainbow(nrow(contingency_table)), main = paste("Barplot of", col1, "vs", col2), xlab = col1, ylab = "Count", legend = TRUE)
    dev.off()
    
    # Mosaicplot
    mosaicplot_file <- paste0("bivariant/mosaicplots/mosaicplot_", col1, "_", col2, ".png")
    png(mosaicplot_file, width = 800, height = 600)
    mosaicplot(contingency_table, main = paste("Mosaic Plot of", col1, "vs", col2), color = TRUE, shade = TRUE, las = 2)
    dev.off()
  }
}

print("Done")
