library(tidyverse)
library(stringr)
library(dplyr)
library(geosphere)

# read in data
sf_airbnb_data <- read.csv("/Users/alexandercho/sf_airbnb/listings.csv", stringsAsFactors = FALSE)
sapply(sf_airbnb_data, function(col) is.integer(col))

# Select only the specified columns
sf_airbnb_data <- select(sf_airbnb_data,
                         host_response_time,
                         host_response_rate,
                         host_acceptance_rate,
                         neighbourhood_cleansed,
                         room_type,
                         accommodates,
                         bathrooms_text,
                         beds,
                         price,
                         review_scores_rating, 
                         availability_365
)

# converts the 'bathrooms_text' character column into an integer
sf_airbnb_data <- sf_airbnb_data %>%
  mutate(bathrooms_number = as.integer(str_extract(bathrooms_text, "\\d+")))

# Delete the "bathrooms_text" column
sf_airbnb_data <- select(sf_airbnb_data, -bathrooms_text)

# look for missing values
colSums(is.na(sf_airbnb_data))

# upon further inspection of a sample of listings from the original data, the high number of missing values for the ratings
# indicates that listings are very new so they will probably not be helpful for the analysis. We'll take care of the other two as well.
# sf_airbnb_data <- sf_airbnb_data[complete.cases(sf_airbnb_data$review_scores_rating, sf_airbnb_data$bathrooms_number, sf_airbnb_data$beds), ]

# Remove non-numeric characters from the following columns and convert them to an int/num
sf_airbnb_data$price <- as.integer(gsub("[^0-9.]", "", sf_airbnb_data$price))
sf_airbnb_data$host_response_rate <- as.numeric(gsub("[^0-9.]", "", sf_airbnb_data$host_response_rate))
sf_airbnb_data$host_acceptance_rate <- as.numeric(gsub("[^0-9.]", "", sf_airbnb_data$host_acceptance_rate))

# Impute NA values with the mean of the column
sf_airbnb_data$host_response_rate[is.na(sf_airbnb_data$host_response_rate)] <- mean(sf_airbnb_data$host_response_rate, na.rm = TRUE)
sf_airbnb_data$host_acceptance_rate[is.na(sf_airbnb_data$host_acceptance_rate)] <- mean(sf_airbnb_data$host_acceptance_rate, na.rm = TRUE)
sf_airbnb_data$beds[is.na(sf_airbnb_data$beds)] <- mean(sf_airbnb_data$beds, na.rm = TRUE)
sf_airbnb_data$review_scores_rating[is.na(sf_airbnb_data$review_scores_rating)] <- mean(sf_airbnb_data$review_scores_rating, na.rm = TRUE)
sf_airbnb_data$bathrooms_number[is.na(sf_airbnb_data$bathrooms_number)] <- mean(sf_airbnb_data$bathrooms_number, na.rm = TRUE)

unique(sf_airbnb_data$host_response_time)
# see the numbers we're working with to see how to replace currently null values
sum(sf_airbnb_data$host_response_time == "within an hour") # 3860
sum(sf_airbnb_data$host_response_time == "within a few hours") # 891
sum(sf_airbnb_data$host_response_time == "within a day") # 392
sum(sf_airbnb_data$host_response_time == "a few days or more") # 64
sum(sf_airbnb_data$host_response_time == "N/A") # 872
sum(sf_airbnb_data$host_response_time == "") # 1

# for the "N/A" strings" and "", replace with 
sf_airbnb_data <- sf_airbnb_data %>% 
  mutate(host_response_time = ifelse(host_response_time %in% c("N/A", ""), "within an hour", host_response_time))

# character variables `host_response_time` and `room_type` as factors
sf_airbnb_data$host_response_time <- as.factor(sf_airbnb_data$host_response_time)
sf_airbnb_data$room_type <- as.factor(sf_airbnb_data$room_type)

# run to view internal structure of dataset 
str(sf_airbnb_data)

colSums(is.na(sf_airbnb_data))







