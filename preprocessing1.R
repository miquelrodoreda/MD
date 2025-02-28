# install.packages("RColorBrewer")
library(RColorBrewer)
library(dplyr)
# install.packages("stringr")
library(stringr)
# install.packages("VIM")
library(VIM)

library(stringi)

setwd("/Users/miquelrodoreda/uni/MD")

filename <- "dataset/filtered_data.csv"
file.exists(filename)
dd <- read.csv(filename)
dd <- dd[, c("price_level", "vegan_options", "awards", "gluten_free", "cuisines", "original_location", "open_days_per_week", "avg_rating", "total_reviews_count", "food", "service", "atmosphere", "excellent", "meals")]

barplot(table(dd$original_location))

# ------------------------------------- original_location cleaning -------------------------------------
unique(dd$original_location)

dd <- dd %>%
  mutate(municipi = str_extract(original_location, "[^,]+(?=\\]$)") %>%
           str_replace_all('""', '') %>%
           str_remove_all('"') %>%
           str_remove_all('\'') %>%
           str_remove_all(' ') %>%
           str_remove_all('-') %>%
           #str_remove_all('\\') %>%
           str_trim() %>%
           stri_trans_general(id = "Latin-ASCII") %>%
           str_to_lower())
print(dd$municipi)

municipis_comarques <- read.csv("dataset/comarcas.csv")

municipis_comarques$Nom.del.municipi <- stri_trans_general(str = municipis_comarques$Nom.del.municipi, id = "Latin-ASCII")
municipis_comarques <- municipis_comarques %>%
  mutate(Nom.del.municipi = str_replace_all(Nom.del.municipi, '""', '') %>%
           str_remove_all('"') %>%
           str_remove_all('\'') %>%
           str_remove_all(' ') %>%
           str_remove_all('-') %>%
           str_trim() %>%
           stri_trans_general(id = "Latin-ASCII") %>%
           str_to_lower())

municipis_comarques$Nom.del.municipi

dd <- dd %>%
  left_join(municipis_comarques, by = c("municipi" = "Nom.del.municipi")) %>%
  mutate(Nom.de.la.comarca = ifelse(is.na(Nom.de.la.comarca), "NotFound", Nom.de.la.comarca))

table(dd$Nom.de.la.comarca)

dd$original_location[dd$Nom.de.la.comarca == "NotFound"]
dd$municipi[dd$Nom.de.la.comarca == "NotFound"]

dd <- dd[, c("price_level", "vegan_options", "awards", "gluten_free", "cuisines", "Nom.de.la.comarca", "open_days_per_week", "avg_rating", "total_reviews_count", "food", "service", "atmosphere", "excellent", "meals")]

dd <- dd %>%
  rename(original_location = Nom.de.la.comarca)

print(dd$original_location)

unique(dd$original_location)

# ------------------------------------- price_level cleaning -------------------------------------

unique(dd$price_level)
dd <- dd %>%
  mutate(price_level = recode(price_level, 
                              "€" = "low", 
                              "€€-€€€" = "medium", 
                              "€€€€" = "high"))
unique(dd$price_level)

# ------------------------------------- meals cleaning -------------------------------------

unique(dd$meals)

dd <- dd %>%
  mutate(meals = case_when(
    meals == "" ~ "NoAnswer",
    str_detect(meals, "Breakfast") & str_detect(meals, "Lunch") & str_detect(meals, "Dinner") ~ "AllMeals",
    str_detect(meals, "Breakfast") & str_detect(meals, "Lunch") ~ "BreakfastLunch",
    str_detect(meals, "Lunch") & str_detect(meals, "Dinner") ~ "LunchDinner",
    str_detect(meals, "Breakfast") & str_detect(meals, "Dinner") ~ "BreakfastDinner",
    str_detect(meals, "Drinks") ~ "Drinks",
    str_detect(meals, "Brunch") | str_detect(meals, "Snack") ~ "LightMeals",
    TRUE ~ "Others"
  ))

unique(dd$meals)

# ------------------------------------- service cleaning -------------------------------------

unique(dd$service)

dd$service[is.na(dd$service)] <- median(dd$service, na.rm=TRUE)

unique(dd$service)

# ------------------------------------- food cleaning -------------------------------------

unique(dd$food)

dd$food[is.na(dd$food)] <- median(dd$food, na.rm=TRUE)

unique(dd$food)

# ------------------------------------- atmosphere cleaning -------------------------------------

unique(dd$atmosphere)

numerics <- unlist(lapply(dd, is.numeric), use.names = FALSE)
cor(dd[!is.na(dd$atmosphere), numerics])

var(dd$atmosphere, na.rm = TRUE)

for (k_value in c(2, 3, 5, 10)) { 
  dd_imputed <- kNN(dd, variable = "atmosphere", k = k_value, dist_var = c("service", "food", "avg_rating"), imp_var = TRUE) 
  print(paste("k =", k_value, " - atmosphere variance:", var(dd_imputed$atmosphere, na.rm = TRUE))) 
}

dd <- kNN(dd, variable = "atmosphere", k = 3, dist_var = c("service", "food", "avg_rating"), imp_var = FALSE)


unique(dd$atmosphere)


# ------------------------------------- saving -------------------------------------

write.table(dd, file = "dataset/preprocessing1.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)