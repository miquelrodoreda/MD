# ==============================
# METADATA MANAGEMENT SCRIPT
# ==============================
# This script manages metadata information for the project, including
# variable descriptions and data dictionaries.

# ==============================
# WORKING DIRECTORY & DATA LOADING
# ==============================
setwd("/Users/miquelrodoreda/uni/MD")

library(codebookr)
library(dplyr)

directory <- "metadata/"
if (!dir.exists(directory)) {
  dir.create(directory, recursive = TRUE)
}

before <- read.csv("dataset/filtered_data.csv")
after <- read.csv("dataset/renamed.csv")

# ==============================
# CODEBOOK GENERATION
# ==============================

glimpse(before)
glimpse(after)

print(x = codebook(before), target = paste0(directory, "before_preprocessing.docx"))
print(x = codebook(after), target = paste0(directory, "after_preprocessing.docx"))
