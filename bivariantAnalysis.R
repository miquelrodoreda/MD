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

print("Done")
