library(tidyverse)
library(tidymodels)
library(stringr)
library(dplyr)


# read in data
sf_airbnb_data <- read.csv("/Users/alexandercho/sf_airbnb/listings.csv", stringsAsFactors = FALSE)
sf_airbnb_raw <- read.csv("/Users/alexandercho/sf_airbnb/listings.csv", stringsAsFactors = FALSE)
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

# IQR, price
price_q1 <- quantile(sf_airbnb_data$price, 0.25)
price_q3 <- quantile(sf_airbnb_data$price, 0.75)
price_iqr <- price_q3 - price_q1
# outliers
outlier_rows <- sf_airbnb_data[sf_airbnb_data$price < (price_q1 - 1.5 * price_iqr) | sf_airbnb_data$price > (price_q3 + 1.5 * price_iqr), ]
nrow(outlier_rows)
# final decision: remove all greater than 1000
sf_airbnb_data <- sf_airbnb_data[sf_airbnb_data$price <= 1000, ]

# new col 'number_of_days_booked'
sf_airbnb_data$number_of_days_booked <- 365 - sf_airbnb_data$availability_365

# Remove the availability_365 column
sf_airbnb_data <- select(sf_airbnb_data, -availability_365)

# missingness
colSums(is.na(sf_airbnb_data))

sf_airbnb_data <- sf_airbnb_data %>%
  mutate(neighbourhood_cleansed = case_when(
    neighbourhood_cleansed %in% c("Seacliff", "Presidio", "Presidio Heights") ~ "Presidio",
    neighbourhood_cleansed %in% c("Twin Peaks", "West of Twin Peaks", "Diamond Heights", "Glen Park") ~ "Twin Peaks",
    neighbourhood_cleansed %in% c("Inner Richmond", "Outer Richmond") ~ "Richmond",
    neighbourhood_cleansed %in% c("Crocker Amazon", "Visitacion Valley", "Excelsior", "Outer Mission") ~ "Excelsior",
    neighbourhood_cleansed %in% c("Inner Sunset", "Outer Sunset", "Golden Gate Park") ~ "Sunset",
    TRUE ~ neighbourhood_cleansed
  ))

# internal structure of dataset 
str(sf_airbnb_data)

table(sf_airbnb_data$neighbourhood_cleansed)

write_csv(sf_airbnb_data, "sf_airbnb_clean.csv")

unique(sf_airbnb_data$bathrooms_number)
# 1.5 bathrooms is showing up as 1.285928...

unique(sf_airbnb_clean$neighbourhood_cleansed)


