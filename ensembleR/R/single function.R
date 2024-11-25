library(caret)
library(ranger)
library(xgboost)
library(e1071)  # for SVM

fit_models <- function(data) {
  # Add Age column and remove Rings column
  data$Age <- data$Rings + 1.5
  data$Rings <- NULL
  
  # Convert Sex column to numeric (one-hot encoding)
  dv <- caret::dummyVars(~ Sex, data = data, fullRank = TRUE)
  dummy_vars <- predict(dv, data)
  keep_names <- setdiff(colnames(data), c("Age", "Sex"))
  
  # Combine original data with one-hot encoded variables to get feature matrix
  data_features <- as.data.frame(cbind(data[, keep_names], dummy_vars))
  
  # Split data into training (80%) and validation (20%) sets
  set.seed(42)
  train_idx <- sample(1:nrow(data), 0.8 * nrow(data))
  train_df <- data[train_idx, ]
  val_df <- data[-train_idx, ]
  
  # Split feature matrix based on training and validation indices
  train_features <- data_features[train_idx, ]
  val_features <- data_features[-train_idx, ]
  
  # 1. Linear Regression Model
  lm_fit <- lm(Age ~ ., data = train_df)
  
  # 2. Random Forest Model
  rf_fit <- ranger::ranger(Age ~ ., data = train_df, num.trees = 256)
  
  # 3. XGBoost Model
  xgb_train <- xgb.DMatrix(data = as.matrix(train_features), label = train_df$Age)
  xgb_fit <- xgboost::xgboost(data = xgb_train, objective = "reg:squarederror", nrounds = 500, verbose = 0)
  
  # 4. SVM Regression Model
  svm_train_features <- as.matrix(train_features)
  svm_fit <- svm(x = svm_train_features, y = train_df$Age, type = "eps-regression", kernel = "radial", cost = 1)
  
  # Return a list of the fitted models
  list(
    lm_model = lm_fit,
    rf_model = rf_fit,
    xgb_model = xgb_fit,
    svm_model = svm_fit
  )
}