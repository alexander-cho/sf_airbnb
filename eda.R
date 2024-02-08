library(ggplot2)
library(corrplot)
library(GGally)

# plot price distribution
price_distribution_plot <- ggplot(sf_airbnb_data, aes(x = price)) + 
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(title = "Price Distribution", x = "Price", y = "Frequency") + 
  theme_minimal()

# correlation matrix
correlation_matrix <- sf_airbnb_data %>%
  select_if(is.numeric) %>%
  cor()

corrplot(correlation_matrix, method = "color")

# pair plot
pair_plot <- ggpairs(sf_airbnb_data[, c("price", "accommodates", "beds", "review_scores_rating")])

# price distribution across neighborhoods
price_dist_neighborhoods_plot <- ggplot(sf_airbnb_data, aes(x = price, y = neighbourhood_cleansed)) +
  geom_boxplot() +
  labs(title = "Price Distribution Across Neighborhoods")

# number of days booked across neighborhoods
days_booked_plot <- ggplot(sf_airbnb_data, aes(x = number_of_days_booked, y = neighbourhood_cleansed)) +
  geom_boxplot() +
  labs(title = "Number of Days Unit is Booked Across Neighborhoods")

# median price by neighborhood
median_price <- median_price %>%
  mutate(neighbourhood_cleansed = reorder(neighbourhood_cleansed, -median_price))

price_by_n_plot <- ggplot(median_price, aes(x = neighbourhood_cleansed, y = median_price)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Median Prices by Neighborhood",
       x = "Neighborhood",
       y = "Median Rent Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

