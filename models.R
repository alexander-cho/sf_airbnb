library(tidymodels)
library(tidyverse)
library(kknn)

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

# create 10-fold cross-validation object
sf_airbnb_folds <- vfold_cv(sf_airbnb_train, v = 10)

sf_airbnb_recipe <- recipe(price ~ ., data = sf_airbnb_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors())

str(sf_airbnb_recipe)


# Linear Regression

# specify linear regression model
lm_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# specify workflow
lm_workflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(sf_airbnb_recipe)

# fit model using cross-validation
lm_fit <- lm_workflow %>%
  fit_resamples(sf_airbnb_folds)

# select the best model and fit
lm_final_fit <- lm_fit %>%
  select_best("rmse") %>%
  finalize_workflow(lm_workflow, .) %>%
  fit(sf_airbnb_train)

# plot actual vs. predicted scores
augment(lm_final_fit, new_data = sf_airbnb_test) %>% 
  ggplot(aes(x = .pred, y = price)) +
  geom_point(alpha = 0.4) +
  geom_abline(lty = 2, color = "red") +
  coord_cartesian(xlim = c(0, 1000),
                  ylim = c(0, 1000)) +
  labs(title = "Linear model: actual vs. predicted prices",
       x = "Predicted price",
       y = "Actual price")

# calculate RMSE
augment(lm_final_fit, new_data = sf_airbnb_test) %>%
  rmse(truth = price, estimate = .pred)

# final linreg model
saveRDS(lm_final_fit, file = "models/lm_model.rds")

# K-nearest neighbors

# specify KNN model
knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_mode("regression") %>%
  set_engine("kknn")

# specify workflow
knn_workflow <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(sf_airbnb_recipe)

# specify hyperparameters
neighbors_grid <- grid_regular(neighbors(range = c(5, 50)),
                               levels = 10)

# fit model using cross-validation and hyperparameters
knn_tune_res <- tune_grid(knn_workflow,
                          resamples = sf_airbnb_folds,
                          grid = neighbors_grid)

# plot results
autoplot(knn_tune_res)

# select the best model and fit
knn_final_fit <- knn_tune_res %>%
  select_best("rmse") %>%
  finalize_workflow(knn_workflow, .) %>%
  fit(sf_airbnb_train)

# plot actual vs. predicted scores
augment(knn_final_fit, new_data = sf_airbnb_test) %>% 
  ggplot(aes(x = .pred, y = price)) +
  geom_point(alpha = 0.4) +
  geom_abline(lty = 2, color = "red") +
  coord_cartesian(xlim = c(0, 1000),
                  ylim = c(0, 1000)) +
  labs(title = "KNN model: actual vs. predicted prices",
       x = "Predicted price",
       y = "Actual price")

# calculate RMSE
augment(knn_final_fit, new_data = sf_airbnb_test) %>%
  rmse(truth = price, estimate = .pred)

# final knn model
saveRDS(knn_final_fit, file = "models/knn_model.rds")
saveRDS(knn_tune_res, file = "models/knn_tune_res.rds")



