# ==============================
# COLUMN RENAMING SCRIPT
# ==============================
# This script standardizes column names in the dataset by shortening
# location names, awards, and cuisines for better readability and analysis.

# ==============================
# LIBRARIES
# ==============================

# Load required libraries
library(RColorBrewer)
library(dplyr)
library(stringr)
library(VIM)
library(stringi)

# ==============================
# WORKING DIRECTORY & DATA LOADING
# ==============================

# Set working directory
setwd("/Users/miquelrodoreda/uni/MD")

# Load dataset
filename <- "dataset/preprocessed.csv"
file.exists(filename)
dd <- read.csv(filename)

# ==============================
# LOCATION NAME SHORTENING
# ==============================

unique(dd$original_location)

dd <- dd %>%
  mutate(original_location = case_when(
    original_location == "Vallès Occidental" ~ "VOc",
    original_location == "Maresme" ~ "Mar",
    original_location == "NotFound" ~ "NF",
    original_location == "Osona" ~ "Os",
    original_location == "Vallès Oriental" ~ "VOr",
    original_location == "Berguedà" ~ "Ber",
    original_location == "Bages" ~ "Bag",
    original_location == "Alt Penedès" ~ "APe",
    original_location == "Baix Llobregat" ~ "BLl",
    original_location == "Barcelonès" ~ "Bar",
    original_location == "Garraf" ~ "Ga",
    original_location == "Moianès" ~ "Mo",
    original_location == "Lluçanès" ~ "Llu",
    original_location == "Anoia" ~ "An",
    TRUE ~ "UNK"
  ))

unique(dd$original_location)

# ==============================
# AWARDS NAME SHORTENING
# ==============================

unique(dd$awards)

dd <- dd %>%
  mutate(awards = case_when(
    awards == "Certificate of Excellence 2011" ~ "CoE11",
    awards == "Certificate of Excellence 2012" ~ "CoE12",
    awards == "Certificate of Excellence 2013" ~ "CoE13",
    awards == "Certificate of Excellence 2014" ~ "CoE14",
    awards == "Certificate of Excellence 2015" ~ "CoE15",
    awards == "Certificate of Excellence 2016" ~ "CoE16",
    awards == "Certificate of Excellence 2017" ~ "CoE17",
    awards == "Certificate of Excellence 2018" ~ "CoE18",
    awards == "Certificate of Excellence 2019" ~ "CoE19",
    awards == "Certificate of Excellence 2020" ~ "CoE20",
    awards == "Not Awarded" ~ "NotAw",
    TRUE ~ "UNK"
  ))

unique(dd$awards)

# ==============================
# CUISINES NAME SHORTENING
# ==============================

unique(dd$cuisines)

dd <- dd %>%
  mutate(cuisines = case_when(
    cuisines == "European" ~ "EU",
    cuisines == "Asian" ~ "AS",
    cuisines == "Latin American" ~ "LA",
    cuisines == "American" ~ "AM",
    cuisines == "Healthy" ~ "HE",
    cuisines == "Fusion / International" ~ "F/I",
    cuisines == "Others" ~ "O",
    TRUE ~ "UNK"
  ))

unique(dd$cuisines)

# ==============================
# SAVE RENAMED DATA
# ==============================

write.table(dd, 
            file = "dataset/renamed.csv", 
            sep = ",", 
            na = "NA", 
            dec = ".", 
            row.names = FALSE, 
            col.names = TRUE)

# Display first few rows of the final dataset
head(dd)

