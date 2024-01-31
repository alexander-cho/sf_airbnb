library(ggplot2)
library(corrplot)
library(GGally)

# plot price distribution
ggplot(sf_airbnb_data, aes(x = price)) + 
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(title = "Price Distribution", x = "Price", y = "Frequency") + 
  theme_minimal()

# correlation matrix
correlation_matrix <- sf_airbnb_data %>%
  select_if(is.numeric) %>%
  cor()

corrplot(correlation_matrix, method = "color")

# pair plot
ggpairs(sf_airbnb_data[, c("price", "accommodates", "beds", "review_scores_rating")])

# price distribution across neighborhoods
ggplot(sf_airbnb_data, aes(x = price, y = neighbourhood_cleansed)) +
  geom_boxplot() +
  labs(title = "Price Distribution Across Neighborhoods")



