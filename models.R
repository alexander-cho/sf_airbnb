library(tidymodels)
library(tidyverse)
# read in clean data
sf_airbnb_clean <- read_csv("sf_airbnb_clean.csv")

set.seed(1)

# chr to factor
sf_airbnb_clean <- sf_airbnb_clean %>%
  mutate(
    neighbourhood_cleansed = as.factor(neighbourhood_cleansed),
    host_response_time = as.factor(host_response_time),
    room_type = as.factor(room_type)
  )

# split into train/test sets
sf_airbnb_split <- initial_split(sf_airbnb_clean, strata = "price", prop = 0.8)
sf_airbnb_train <- training(sf_airbnb_split)
sf_airbnb_test <- testing(sf_airbnb_split)
