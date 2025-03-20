setwd("/Users/miquelrodoreda/uni/MD")

data <- read.csv("dataset/tripadvisor_european_restaurants.csv")

filtered_data <- data[data$province == "Province of Barcelona" & data$total_reviews_count > 100, ]

dim(filtered_data)

