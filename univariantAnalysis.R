# UnivariantAnalysis.R
# This script performs univariate analysis on the dataset contained in "cleaned.csv".
# For each numeric variable, it generates a histogram and a boxplot.
# For each categorical variable, it generates a barplot.
# All plots are saved in the "univariant/plots" folder.

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Optionally, set the working directory (uncomment and adjust the path if needed)
# setwd("/path/to/your/project/folder")

# Read the dataset from cleaned.csv
data_file <- "dataset/preprocessed.csv"
dd <- read.table(data_file, header = TRUE, sep = ",", fill = TRUE)

# Display a summary of the dataset
print(dd)
cat("Dimensions: ", dim(dd), "\n")
n <- nrow(dd)
K <- ncol(dd)
cat("Number of observations: ", n, "\n")
cat("Number of variables: ", K, "\n")
print(names(dd))

# Define output directory for all plots
plots_dir <- "univariant_after/plots"

# Create the output directory if it doesn't exist
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

# Identify numeric and categorical variables
numeric_vars <- names(dd)[sapply(dd, is.numeric)]
categorical_vars <- names(dd)[sapply(dd, function(x) is.character(x) || is.factor(x))]

cat("Numeric variables: ", paste(numeric_vars, collapse = ", "), "\n")
cat("Categorical variables: ", paste(categorical_vars, collapse = ", "), "\n")

# Function to generate and save histogram and boxplot for numeric variables
plot_numeric <- function(data, var_name, out_dir, counter) {
  data_var <- data[[var_name]]
  # Remove NA values
  data_var <- data_var[!is.na(data_var)]
  if (length(data_var) == 0) {
    cat("No available data for numeric variable:", var_name, "\n")
    return()
  }
  
  # Global summary
  print(summary(data_var))
  cat("\n")
  
  # Dynamically determine bin width for the histogram
  bin_width <- (max(data_var) - min(data_var)) / 30
  
  # Create histogram plot
  p_hist <- ggplot(data, aes_string(x = var_name)) +
    geom_histogram(binwidth = bin_width, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram of", var_name), x = var_name, y = "Frequency") +
    theme_minimal()
  
  # Save histogram to file with numbered filename
  hist_file <- file.path(out_dir, paste0("histogram", counter, ".png"))
  ggsave(hist_file, plot = p_hist, width = 7, height = 5)
  
  # Create boxplot
  p_box <- ggplot(data, aes_string(y = var_name)) +
    geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot of", var_name), y = var_name) +
    theme_minimal()
  
  # Save boxplot to file with numbered filename
  box_file <- file.path(out_dir, paste0("boxplot", counter, ".png"))
  ggsave(box_file, plot = p_box, width = 7, height = 5)
  
  cat("Plots saved for numeric variable:", var_name, "\n")
}

# Function to generate and save barplot for categorical variables
plot_categorical <- function(data, var_name, out_dir, counter) {
  data_var <- data[[var_name]]
  # Remove NA values
  data_var <- data_var[!is.na(data_var)]
  if (length(data_var) == 0) {
    cat("No available data for categorical variable:", var_name, "\n")
    return()
  }
  
  # Frequency table
  print(table(data_var))
  
  # Create barplot
  p <- ggplot(data, aes_string(x = var_name)) +
    geom_bar(fill = "purple", color = "black", alpha = 0.7) +
    labs(title = paste("Barplot of", var_name), x = var_name, y = "Count") +
    theme_minimal() +
    coord_flip()
  
  # Save barplot to file with numbered filename
  bar_file <- file.path(out_dir, paste0("barplot", counter, ".png"))
  ggsave(bar_file, plot = p, width = 7, height = 5)
  
  cat("Plot saved for categorical variable:", var_name, "\n")
}

# Initialize counters for numeric and categorical plots
numeric_counter <- 1
categorical_counter <- 1

# Generate plots for all numeric variables
for (var in numeric_vars) {
  plot_numeric(dd, var, plots_dir, numeric_counter)
  numeric_counter <- numeric_counter + 1
}

# Generate plots for all categorical variables
for (var in categorical_vars) {
  plot_categorical(dd, var, plots_dir, categorical_counter)
  categorical_counter <- categorical_counter + 1
}

cat("Univariate analysis completed.\n")
