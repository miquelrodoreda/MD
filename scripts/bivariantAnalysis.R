# ==============================
# LIBRARIES
# ==============================

# Install required packages if not already installed
# install.packages("psych")
library(psych)
library(RColorBrewer)
library(ggplot2)

# ==============================
# WORKING DIRECTORY & DATA LOADING
# ==============================

# Set working directory
setwd("/Users/miquelrodoreda/uni/MD")

# Define dataset file and output directory
filename <- "dataset/preprocessed.csv"
base_output_dir <- "bivariant_after/"

# Load dataset
dd <- read.csv(filename)

# Select relevant columns for analysis
dd <- dd[, c("price_level", "vegan_options", "awards", "gluten_free", "cuisines", "original_location", 
             "open_days_per_week", "avg_rating", "total_reviews_count", "food", "service", 
             "atmosphere", "excellent", "meals")]

# ==============================
# OUTPUT DIRECTORIES CREATION
# ==============================

# Define subdirectories for different types of plots
output_dirs <- c(paste0(base_output_dir, "scatterplots"), 
                 paste0(base_output_dir, "boxplots"), 
                 paste0(base_output_dir, "histograms"), 
                 paste0(base_output_dir, "barplots"), 
                 paste0(base_output_dir, "mosaicplots"), 
                 paste0(base_output_dir, "contingency_table"))

# Create directories if they don't already exist
for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# ==============================
# IDENTIFY NUMERIC & CATEGORICAL VARIABLES
# ==============================

# Identify numeric columns
num_cols <- names(dd)[sapply(dd, is.numeric)]

# Identify categorical columns (character or factor type)
cat_cols <- names(dd)[sapply(dd, function(col) is.character(col) || is.factor(col))]

# ==============================
# PAIRWISE COLUMN COMBINATIONS
# ==============================

# Generate all unique column pairs (excluding self-pairing)
column_pairs <- expand.grid(names(dd), names(dd), stringsAsFactors = FALSE)
column_pairs <- column_pairs[column_pairs[, 1] != column_pairs[, 2], ]

# ==============================
# ITERATE OVER COLUMN PAIRS TO GENERATE PLOTS
# ==============================

for (i in seq_len(nrow(column_pairs))) {
  col1 <- column_pairs[i, 1]
  col2 <- column_pairs[i, 2]
  
  # Skip duplicate combinations (to avoid reversed duplicates)
  if (col1 > col2) next
  
  # ==============================
  # SCATTERPLOTS: NUMERIC vs NUMERIC
  # ==============================
  
  if (col1 %in% num_cols && col2 %in% num_cols) {
    file_name <- paste0(base_output_dir, "scatterplots/scatterplot_", col1, "_", col2, ".png")
    png(file_name, width = 800, height = 600)
    pairs.panels(dd[, c(col1, col2)], method = "pearson", hist.col = "lightblue", density = TRUE, ellipses = TRUE)
    dev.off()
  }
  
  # ==============================
  # BOXPLOTS & HISTOGRAMS: NUMERIC vs CATEGORICAL
  # ==============================
  
  if ((col1 %in% num_cols && col2 %in% cat_cols) || (col1 %in% cat_cols && col2 %in% num_cols)) {
    if (col1 %in% num_cols) {
      numeric_col <- col1
      categorical_col <- col2
    } else {
      numeric_col <- col2
      categorical_col <- col1
    }
    
    # Remove rows with missing values in either column
    valid_data <- dd[!is.na(dd[[col1]]) & !is.na(dd[[col2]]), ]
    
    # Print column pair being analyzed
    print(paste0(col1, "-", col2))
    
    # Summary statistics grouped by categorical variable
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
    
    # Generate Boxplot
    boxplot_file <- paste0(base_output_dir, "boxplots/boxplot_", col1, "_", col2, ".png")
    png(boxplot_file, width = 800, height = 600)
    if (col1 %in% num_cols) {
      boxplot(valid_data[[col1]] ~ valid_data[[col2]], main = paste("Boxplot of", col1, "by", col2), xlab = col2, ylab = col1, col = "lightblue", border = "black")
    } else {
      boxplot(valid_data[[col2]] ~ valid_data[[col1]], main = paste("Boxplot of", col2, "by", col1), xlab = col1, ylab = col2, col = "lightblue", border = "black")
    }
    dev.off()
    
    # Generate Histogram
    hist_file <- paste0(base_output_dir, "histograms/histogram_", col1, "_", col2, ".png")
    
    # Define colors for different categories
    categories <- unique(valid_data[[categorical_col]])
    num_categories <- length(categories)
    category_colors <- colorRampPalette(brewer.pal(12, "Set3"))(num_categories)
    
    ggplot(valid_data, aes(x = .data[[numeric_col]], fill = .data[[categorical_col]], y = after_stat(density))) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
      labs(title = paste("Overlayed Histogram of", numeric_col, "grouped by", categorical_col),
           x = numeric_col, y = "Density", fill = categorical_col) +
      theme_minimal()
    
    # Save Histogram plot
    ggsave(hist_file, width = 8, height = 6, dpi = 300)
    
  }
  
  # ==============================
  # BARPLOTS & MOSAICPLOTS: CATEGORICAL vs CATEGORICAL
  # ==============================
  
  if (col1 %in% cat_cols && col2 %in% cat_cols) {
    # Create contingency table
    contingency_table <- table(dd[[col1]], dd[[col2]])
    df_table <- as.data.frame(as.table(contingency_table))
    
    # Generate Heatmap (Contingency Table)
    p <- ggplot(df_table, aes(x = Var2, y = Var1, fill = Freq)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(title = "Contingency Table Heatmap", x = col1, y = col2, fill = "Count") +
      theme_minimal()
    
    # Save Contingency Table Heatmap
    contingency_table_file <- paste0(base_output_dir, "contingency_table/contingency_table_", col1, "_", col2, ".png")
    ggsave(contingency_table_file, plot = p, width = 8, height = 6, dpi = 300)
    
    # Generate Barplot
    barplot_file <- paste0(base_output_dir, "barplots/barplot_", col1, "_", col2, ".png")
    png(barplot_file, width = 800, height = 600)
    barplot(contingency_table, beside = TRUE, col = rainbow(nrow(contingency_table)), main = paste("Barplot of", col1, "vs", col2), xlab = col1, ylab = "Count", legend = TRUE)
    dev.off()
    
    # Generate Mosaic Plot
    mosaicplot_file <- paste0(base_output_dir, "mosaicplots/mosaicplot_", col1, "_", col2, ".png")
    png(mosaicplot_file, width = 800, height = 600)
    mosaicplot(contingency_table, main = paste("Mosaic Plot of", col1, "vs", col2), color = TRUE, shade = TRUE, las = 2)
    dev.off()
  }
}

print("Done")
