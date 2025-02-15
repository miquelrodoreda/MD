setwd("/home/gerard/Desktop/MD")

data <- read.csv("data/tripadvisor_european_restaurants.csv")

filtered_data <- data[data$city == "Madrid", ]

filter_review_count_data <- filtered_data[filtered_data$total_reviews_count > 200, ]

spanish_cuisine_data <- filtered_data[grepl("Spanish", filtered_data$cuisines, ignore.case = TRUE), ]

spanish_exclusive_cuisine_data <- filtered_data[filtered_data$cuisines == "Spanish", ]

mediterranean_exclusive_cuisine_data <- filtered_data[filtered_data$cuisines == "Mediterranean", ]

barcelona <- data[data$city == "Barcelona", ]
