# ==============================
# DATA PREPROCESSING SCRIPT
# ==============================
# This script performs data preprocessing on the filtered dataset, including:
# - Cleaning and standardizing text fields
# - Handling missing values
# - Recoding categorical variables
# - Classifying cuisines
# - Imputing missing values using kNN

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
filename <- "dataset/filtered_data.csv"
file.exists(filename)
dd <- read.csv(filename)

# Select relevant columns
dd <- dd[, c("price_level", "vegan_options", "awards", "gluten_free", 
             "cuisines", "original_location", "open_days_per_week", 
             "avg_rating", "total_reviews_count", "food", "service", 
             "atmosphere", "excellent", "meals")]

# ==============================
# TEXT CLEANING FUNCTIONS
# ==============================

# Function to clean and standardize text
clean_text <- function(text) {
  text %>%
    str_replace_all('""', '') %>%
    str_remove_all('"') %>%
    str_remove_all('\'') %>%
    str_remove_all(' ') %>%
    str_remove_all('-') %>%
    str_trim() %>%
    stri_trans_general(id = "Latin-ASCII") %>%
    str_to_lower()
}

# ==============================
# LOCATION CLEANING
# ==============================

# Display initial location distribution
barplot(table(dd$original_location))
unique(dd$original_location)

# Extract municipality from location
dd <- dd %>%
  mutate(municipi = str_extract(original_location, "[^,]+(?=\\]$)") %>%
           clean_text())
print(dd$municipi)

# Load and process comarcas data
municipis_comarques <- read.csv("dataset/comarcas.csv")
municipis_comarques <- municipis_comarques %>%
  mutate(Nom.del.municipi = stri_trans_general(Nom.del.municipi, id = "Latin-ASCII") %>%
         clean_text())

# Join with comarcas data
dd <- dd %>%
  left_join(municipis_comarques, by = c("municipi" = "Nom.del.municipi")) %>%
  mutate(Nom.de.la.comarca = ifelse(is.na(Nom.de.la.comarca), "NotFound", Nom.de.la.comarca))

# Check results
table(dd$Nom.de.la.comarca)
dd$original_location[dd$Nom.de.la.comarca == "NotFound"]
dd$municipi[dd$Nom.de.la.comarca == "NotFound"]

# Select and rename columns
dd <- dd[, c("price_level", "vegan_options", "awards", "gluten_free", 
             "cuisines", "Nom.de.la.comarca", "open_days_per_week", 
             "avg_rating", "total_reviews_count", "food", "service", 
             "atmosphere", "excellent", "meals")]

dd <- dd %>%
  rename(original_location = Nom.de.la.comarca)

# ==============================
# PRICE LEVEL CLEANING
# ==============================

unique(dd$price_level)
dd <- dd %>%
  mutate(price_level = recode(price_level, 
                             "€" = "low", 
                             "€€-€€€" = "medium", 
                             "€€€€" = "high"))
unique(dd$price_level)

# ==============================
# MEALS CLEANING
# ==============================

unique(dd$meals)
dd <- dd %>%
  mutate(meals = case_when(
    meals == "" ~ "UNK",
    str_detect(meals, "Breakfast") & str_detect(meals, "Lunch") & str_detect(meals, "Dinner") ~ "AM",
    str_detect(meals, "Breakfast") & str_detect(meals, "Lunch") ~ "BL",
    str_detect(meals, "Lunch") & str_detect(meals, "Dinner") ~ "LD",
    str_detect(meals, "Breakfast") & str_detect(meals, "Dinner") ~ "BD",
    str_detect(meals, "Breakfast") ~ "B",
    str_detect(meals, "Lunch") ~ "L",
    str_detect(meals, "Dinner") ~ "D",
    TRUE ~ "Others"
  ))
unique(dd$meals)

# ==============================
# MISSING VALUES HANDLING
# ==============================

# Service rating
unique(dd$service)
dd$service[is.na(dd$service)] <- median(dd$service, na.rm = TRUE)
unique(dd$service)

# Food rating
unique(dd$food)
dd$food[is.na(dd$food)] <- median(dd$food, na.rm = TRUE)
unique(dd$food)

# Atmosphere rating
unique(dd$atmosphere)
numerics <- unlist(lapply(dd, is.numeric), use.names = FALSE)
cor(dd[!is.na(dd$atmosphere), numerics])
var(dd$atmosphere, na.rm = TRUE)

# Impute atmosphere using kNN
dd <- kNN(dd, 
          variable = "atmosphere", 
          k = 3, 
          dist_var = c("service", "food", "avg_rating"), 
          imp_var = FALSE)
unique(dd$atmosphere)

# ==============================
# AWARDS CLEANING
# ==============================

unique(dd$awards)
dd <- dd %>%
  mutate(awards = str_extract(awards, "Certificate of Excellence \\d{4}"))
unique(dd$awards)
dd$awards[is.na(dd$awards)] <- "Not Awarded"

# ==============================
# CUISINE CLASSIFICATION
# ==============================

unique(dd$cuisines)

# Function to classify cuisines
classify_cuisine <- function(cuisine) {
  if (grepl("Mediterranean|Spanish|Catalan|Italian|French|German|Polish|Portuguese|British|Neapolitan|Sicilian|Tuscan|Belgian|Northern-Italian|Central European|Russian|Swiss|Hungarian|Dutch", cuisine, ignore.case = TRUE)) {
    return("European")
  }
  if (grepl("Chinese|Japanese|Korean|Vietnamese|Thai|Taiwanese|Indonesian|Filipino|Singaporean|Middle Eastern|Azerbaijani|Yunnan|Central Asian|Japanese Fusion|Sushi", cuisine, ignore.case = TRUE)) {
    return("Asian")
  }
  if (grepl("Mexican|Latin|South American|Central American|Argentinian|Brazilian|Peruvian|Chilean|Colombian|Venezuelan|Cuban|Caribbean", cuisine, ignore.case = TRUE)) {
    return("Latin American")
  }
  if (grepl("American|Steakhouse|Barbecue|Fast food|Grill|Diner|Bar|Pub|Gastropub|Wine Bar|Brew Pub|Dining bars", cuisine, ignore.case = TRUE)) {
    return("American")
  }
  if (grepl("Mediterranean|Healthy|Contemporary|Soups|Sardinian|Balti", cuisine, ignore.case = TRUE)) {
    return("Healthy")
  }
  if (grepl("Fusion|International", cuisine, ignore.case = TRUE)) {
    return("Fusion / International")
  }
  return("Others")
}

# Apply cuisine classification
dd$cuisines <- sapply(dd$cuisines, classify_cuisine)
unique(dd$cuisines)

# ==============================
# OPENING DAYS CLEANING
# ==============================

unique(dd$open_days_per_week)
dd$open_days_per_week[is.na(dd$open_days_per_week)] <- median(dd$open_days_per_week, na.rm = TRUE)
unique(dd$open_days_per_week)

# ==============================
# SAVE PREPROCESSED DATA
# ==============================

write.table(dd, 
            file = "dataset/preprocessed.csv", 
            sep = ",", 
            na = "NA", 
            dec = ".", 
            row.names = FALSE, 
            col.names = TRUE)
