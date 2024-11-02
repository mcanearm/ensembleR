install.packages("randomForest")
install.packages("xgboost")
install.packages("ggplot2")
install.packages("caret")
library(randomForest)
library(xgboost)
library(ggplot2)
library(caret)
# Load necessary libraries
set.seed(123)

# Generate dataset, return list and matrix
generate_dataset <- function(n_samples, n_features) {
  features <- matrix(runif(n_samples * n_features), nrow = n_samples)
  target <- sample(0:1, n_samples, replace = TRUE)
  list(features = features, target = target)
}

# Generate datasets with different numbers of features
dataset_10 <- generate_dataset(1000, 10)
dataset_50 <- generate_dataset(1000, 50)
dataset_100 <- generate_dataset(1000, 100)
dataset_200 <- generate_dataset(1000, 200)

datasets <- list(dataset_10, dataset_50, dataset_100, dataset_200)
dataset_names <- c("10 Features", "50 Features", "100 Features", "200 Features")

# Model training and evaluation
train_and_evaluate_rf_xgb <- function(dataset) {
  # Create training and testing sets
  train_index <- createDataPartition(dataset$target, p = 0.7, list = FALSE)
  
  train_features <- dataset$features[train_index, ]
  train_target <- dataset$target[train_index]
  
  test_features <- dataset$features[-train_index, ]
  test_target <- dataset$target[-train_index]
  
  # Random Forest model training and prediction
  rf_model <- randomForest(train_features, as.factor(train_target))
  rf_predictions <- predict(rf_model, test_features)
  
  # XGBoost model training and prediction
  xgb_model <- xgboost(data = as.matrix(train_features), label = train_target,
                       nrounds = 100, objective = "binary:logistic", verbose = FALSE)
  
  xgb_predictions_prob <- predict(xgb_model, as.matrix(test_features))
  xgb_predictions <- ifelse(xgb_predictions_prob > 0.5, 1, 0)
  
  # Return accuracies of both models
  rf_accuracy <- mean(rf_predictions == test_target)
  xgb_accuracy <- mean(xgb_predictions == test_target)
  
  return(c(rf_accuracy, xgb_accuracy))
}

# Dataset benchmarking
results <- data.frame(Dataset = character(), RF_Accuracy = numeric(), XGB_Accuracy = numeric())

for (i in seq_along(datasets)) {
  cat("Running models on", dataset_names[i], "\n")
  
  accuracies <- train_and_evaluate_rf_xgb(datasets[[i]])
  
  results <- rbind(results, data.frame(
    Dataset = dataset_names[i],
    RF_Accuracy = accuracies[1],
    XGB_Accuracy = accuracies[2]
  ))
}

print(results)

