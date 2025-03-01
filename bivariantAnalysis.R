# Libraries
#install.packages("psych")
library(psych)
library(RColorBrewer)
library(ggplot2)

# Work directory
setwd("/Users/miquelrodoreda/uni/MD")

# Read dataset
filename <- "dataset/preprocessed.csv"
base_output_dir <- "bivariant_after/"
dd <- read.csv(filename)
dd <- dd[, c("price_level", "vegan_options", "awards", "gluten_free", "cuisines", "original_location", "open_days_per_week", "avg_rating", "total_reviews_count", "food", "service", "atmosphere", "excellent", "meals")]

output_dirs <- c(paste0(base_output_dir, "scatterplots"), paste0(base_output_dir, "boxplots"), paste0(base_output_dir, "histograms"), paste0(base_output_dir, "barplots"), paste0(base_output_dir, "mosaicplots"), paste0(base_output_dir, "contingency_table"))
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
    file_name <- paste0(base_output_dir, "scatterplots/scatterplot_", col1, "_", col2, ".png")
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
      categorical_col <- col1
    }

    valid_data <- dd[!is.na(dd[[col1]]) & !is.na(dd[[col2]]), ]
    
    print(paste0(col1, "-", col2))
    summary_by_modality <- valid_data %>%
      group_by(.data[[categorical_col]]) %>%
      summarise(
        count = n(),
        mean = mean(.data[[numeric_col]], na.rm = TRUE),
        median = median(.data[[numeric_col]], na.rm = TRUE),
        sd = sd(.data[[numeric_col]], na.rm = TRUE),
        min = min(.data[[numeric_col]], na.rm = TRUE),
        max = max(.data[[numeric_col]], na.rm = TRUE)
      )
    print(summary_by_modality)
    cat("\n")

    # Boxplot
    boxplot_file <- paste0(base_output_dir, "boxplots/boxplot_", col1, "_", col2, ".png")
    png(boxplot_file, width = 800, height = 600)
    if (col1 %in% num_cols) {
      boxplot(valid_data[[col1]] ~ valid_data[[col2]], main = paste("Boxplot of", col1, "by", col2), xlab = col2, ylab = col1, col = "lightblue", border = "black")
    } else {
      boxplot(valid_data[[col2]] ~ valid_data[[col1]], main = paste("Boxplot of", col2, "by", col1), xlab = col1, ylab = col2, col = "lightblue", border = "black")
    }
    dev.off()
    
    # Histogram
    hist_file <- paste0(base_output_dir, "histograms/histogram_", col1, "_", col2, ".png")

    # Get the unique categories in col2
    categories <- unique(valid_data[[categorical_col]])
    num_categories <- length(unique(valid_data[[categorical_col]]))
    category_colors <- colorRampPalette(brewer.pal(12, "Set3"))(num_categories)
    
    ggplot(valid_data, aes(x = .data[[numeric_col]], fill = .data[[categorical_col]], y = after_stat(density))) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
      labs(title = paste("Overlayed Histogram of", numeric_col, "grouped by", categorical_col),
           x = numeric_col, y = "Density", fill = categorical_col) +
      theme_minimal()
    
    # Save the plot
    hist_file <- paste0(base_output_dir, "histograms/histogram_", col1, "_", col2, ".png")
    ggsave(hist_file, width = 8, height = 6, dpi = 300)
    
  }
  
  # Barplots and mosaic plots for categorical vs categorical
  if (col1 %in% cat_cols && col2 %in% cat_cols) {
    contingency_table <- table(dd[[col1]], dd[[col2]])
    df_table <- as.data.frame(as.table(contingency_table))
    p <- ggplot(df_table, aes(x = Var2, y = Var1, fill = Freq)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(title = "Contingency Table Heatmap", x = col1, y = col2, fill = "Count") +
      theme_minimal()
    
    # Save as PNG
    contingency_table_file <- paste0(base_output_dir, "contingency_table/contingency_table_", col1, "_", col2, ".png")
    ggsave(contingency_table_file, plot = p, width = 8, height = 6, dpi = 300)
    
    # Barplot
    barplot_file <- paste0(base_output_dir, "barplots/barplot_", col1, "_", col2, ".png")
    png(barplot_file, width = 800, height = 600)
    barplot(contingency_table, beside = TRUE, col = rainbow(nrow(contingency_table)), main = paste("Barplot of", col1, "vs", col2), xlab = col1, ylab = "Count", legend = TRUE)
    dev.off()
    
    # Mosaicplot
    mosaicplot_file <- paste0(base_output_dir, "mosaicplots/mosaicplot_", col1, "_", col2, ".png")
    png(mosaicplot_file, width = 800, height = 600)
    mosaicplot(contingency_table, main = paste("Mosaic Plot of", col1, "vs", col2), color = TRUE, shade = TRUE, las = 2)
    dev.off()
  }
}

print("Done")
