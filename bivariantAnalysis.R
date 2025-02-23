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
    if (col1 %in% num_cols) {
      hist(valid_data[[col1]], main = paste("Histogram of", col1, "grouped by", col2), xlab = col1, col = "lightblue", border = "black", breaks = 20)
    } else {
      hist(valid_data[[col2]], main = paste("Histogram of", col2, "grouped by", col1), xlab = col2, col = "lightblue", border = "black", breaks = 20)
    }
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
