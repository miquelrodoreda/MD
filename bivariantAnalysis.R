# Libraries
#install.packages("psych")
library(psych)

# Work directory
setwd("/home/joan/Documents/Ricard/UPC/MD")

# Read dataset
filename = "MD/dataset/cleaned.csv"
dd <- read.table(filename, header = TRUE, sep = ",", fill = TRUE)
dd
dim(dd)
n <- dim(dd)[1]
n
K <- dim(dd)[2]
K
names(dd)

# Folder for scatterplots
output_dir <- "bivariant/scatterplots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Filter numeric columns
num_cols_indexes <- c(4, 5, 6, 7, 8, 9, 10)
num_cols <- names(dd)[num_cols_indexes]  # Obtener nombres de las columnas numéricas

# Iterate through all combinations between numeric columns
combinations <- combn(num_cols, 2, simplify = FALSE)

for (i in seq_along(combinations)) {
  cols <- combinations[[i]]
  
  file_name <- paste0(output_dir, "/scatterplot", i, ".png")
  png(file_name, width = 800, height = 600)
  
  pairs.panels(dd[, cols], 
               method = "pearson",
               hist.col = "lightblue", 
               density = TRUE,
               ellipses = TRUE)
  
  dev.off()
}

# Filter numeric columns
cat_cols_indexes <- c(12, 1, 13, 11, 2, 3)
cat_cols <- names(dd)[cat_cols_indexes]  # Obtener nombres de las columnas categoricas

# Folder for boxplots
output_dir <- "bivariant/boxplots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Folder for histograms
output_dir <- "bivariant/histograms"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Iterate through all combinations between numeric and categoric columns
combinations <- expand.grid(num_cols, cat_cols, stringsAsFactors = FALSE)

# Iterate through all combinations between numeric and categorical columns
# Iterate through all combinations between numeric and categorical columns
for (i in seq_len(nrow(combinations))) {
  num_col <- combinations[i, 1]
  cat_col <- combinations[i, 2] 
  
  # Filtrar datos no numéricos o NA
  valid_data <- dd[!is.na(dd[[num_col]]) & !is.na(dd[[cat_col]]), ]

  # Comprobar si num_col sigue siendo numérico
  if (!is.numeric(valid_data[[num_col]])) {
    print(paste("Skipping", num_col, "as it is not numeric"))
    next
  }

  # Comprobar si cat_col sigue siendo categórico
  if (!is.factor(valid_data[[cat_col]]) && !is.character(valid_data[[cat_col]])) {
    print(paste("Skipping", cat_col, "as it is not categorical"))
    next
  }

  # Boxplot
  boxplot_file <- paste0("bivariant/boxplots/boxplot_", num_col, "_", cat_col, ".png")
  png(boxplot_file, width = 800, height = 600)
  
  boxplot(valid_data[[num_col]] ~ valid_data[[cat_col]], 
          main = paste("Boxplot of", num_col, "by", cat_col),
          xlab = cat_col, 
          ylab = num_col,
          col = "lightblue", 
          border = "black")
  
  dev.off()
  
  # Histogram
  hist_file <- paste0("bivariant/histograms/histogram_", num_col, "_", cat_col, ".png")
  png(hist_file, width = 800, height = 600)
  
  hist(valid_data[[num_col]], 
       main = paste("Histogram of", num_col, "grouped by", cat_col),
       xlab = num_col, 
       col = "lightblue", 
       border = "black",
       breaks = 20)
  
  dev.off()
}


# Folder for barplots
barplot_dir <- "bivariant/barplots"
if (!dir.exists(barplot_dir)) {
  dir.create(barplot_dir, recursive = TRUE)
}

# Folder for mosaic plots
mosaicplot_dir <- "bivariant/mosaicplots"
if (!dir.exists(mosaicplot_dir)) {
  dir.create(mosaicplot_dir, recursive = TRUE)
}

# Iterate through all combinations between categorical columns
cat_combinations <- combn(cat_cols, 2, simplify = FALSE)

for (i in seq_along(cat_combinations)) {
  cols <- cat_combinations[[i]]
  cat1 <- cols[1]
  cat2 <- cols[2]
  
  # Create a contingency table
  contingency_table <- table(dd[[cat1]], dd[[cat2]])
  
  # Barplot Bivariant
  barplot_file <- paste0(barplot_dir, "/barplot_", cat1, "_", cat2, ".png")
  png(barplot_file, width = 800, height = 600)
  
  barplot(contingency_table, beside = TRUE, col = rainbow(nrow(contingency_table)),
          main = paste("Barplot of", cat1, "vs", cat2),
          xlab = cat1, ylab = "Count", legend = TRUE)
  
  dev.off()
  
  # Mosaic Plot
  mosaicplot_file <- paste0(mosaicplot_dir, "/mosaicplot_", cat1, "_", cat2, ".png")
  png(mosaicplot_file, width = 800, height = 600)
  
  mosaicplot(contingency_table, main = paste("Mosaic Plot of", cat1, "vs", cat2),
             color = TRUE, shade = TRUE, las = 2)
  
  dev.off()
}

print("Done")
