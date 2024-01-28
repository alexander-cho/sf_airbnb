library(tidyverse)
library(stringr)

# read in data
sf_airbnb_data <- read.csv("/Users/alexandercho/sf_airbnb/listings.csv", stringsAsFactors = FALSE)


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
                         review_scores_rating
)

# converts the 'bathrooms_text' character column into an integer
sf_airbnb_data <- sf_airbnb_data %>%
  mutate(bathrooms_number = as.integer(str_extract(bathrooms_text, "\\d+")))

# Delete the "bathrooms_text" column
sf_airbnb_data <- select(sf_airbnb_data, -bathrooms_text)

# MISSING VALUES - review_scores_rating: 1881, bathrooms_number: 33, beds: 97
colSums(is.na(sf_airbnb_data))

# upon further inspection of a sample of listings from the original data, the high number of missing values for the ratings
# indicates that listings are very new so they will probably not be helpful for the analysis. We'll take care of the other two as well.
sf_airbnb_data <- sf_airbnb_data[complete.cases(sf_airbnb_data$review_scores_rating, sf_airbnb_data$bathrooms_number, sf_airbnb_data$beds), ]


str(sf_airbnb_data)


