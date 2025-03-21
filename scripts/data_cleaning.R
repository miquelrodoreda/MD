# ==============================
# DATA CLEANING SCRIPT
# ==============================
# This script contains functions for cleaning and preparing the dataset,
# including handling missing values and standardizing variables.

# ==============================
# WORKING DIRECTORY & DATA LOADING
# ==============================

# Set working directory
setwd("/Users/miquelrodoreda/uni/MD")

# Load dataset
data <- read.csv("dataset/tripadvisor_european_restaurants.csv")

# ==============================
# DATA FILTERING
# ==============================

# Filter restaurants in Barcelona with more than 100 reviews
filtered_data <- data[data$province == "Province of Barcelona" & 
                     data$total_reviews_count > 100, ]

# Display dimensions of filtered dataset
dim(filtered_data)

