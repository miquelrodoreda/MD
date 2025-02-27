library(RColorBrewer)
library(dplyr)
# install.packages("stringr")
library(stringr)

setwd("/home/carles/Descargas/")

filename <- "filtered_data.csv"
file.exists(filename)
dd <- read.csv(filename)
dd <- dd[, c("price_level", "vegan_options", "awards", "gluten_free", "cuisines", "original_location", "open_days_per_week", "avg_rating", "total_reviews_count", "food", "service", "atmosphere", "excellent", "meals")]

# ------------------------------------- original_location cleaning -------------------------------------
unique(dd$original_location)

dd <- dd %>%
  mutate(original_location = str_extract(original_location, "[^,]+(?=\\]$)") %>%
           str_replace_all('""', '') %>%
           str_remove_all('"') %>%
           str_trim())
print(dd$original_location)

municipios_comarcas <- read.csv("comarcas.csv")

print(municipios_comarcas)

dd <- dd %>%
  left_join(municipios_comarcas, by = c("original_location" = "Nom.del.municipi")) %>%
  mutate(Nom.de.la.comarca = ifelse(is.na(Nom.de.la.comarca), "NotFound", Nom.de.la.comarca))

table(dd$Nom.de.la.comarca)

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

# ------------------------------------- saving -------------------------------------

write.table(dd, file = "preprocessing1.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)