library(caret)
library(ranger)
library(xgboost)
library(e1071)

fit_models <- function(X, Y) {
  # Split the data into training and validation sets
  set.seed(42)
  train_idx <- sample(1:nrow(X), 0.8 * nrow(X))
  train_X <- X[train_idx, ]
  val_X <- X[-train_idx, ]
  train_Y <- Y[train_idx]
  val_Y <- Y[-train_idx]
  
  # 1. Linear Regression Model
  lm_fit <- lm(train_Y ~ ., data = data.frame(train_X, train_Y))
  
  # 2. Random Forest Model
  rf_fit <- ranger::ranger(train_Y ~ ., data = data.frame(train_X, train_Y), num.trees = 256)
  
  # 3. XGBoost Model
  xgb_train <- xgb.DMatrix(data = as.matrix(train_X), label = train_Y)
  xgb_fit <- xgboost::xgboost(data = xgb_train, objective = "reg:squarederror", nrounds = 500, verbose = 0)
  
  # 4. SVM Regression Model
  svm_fit <- svm(x = as.matrix(train_X), y = train_Y, type = "eps-regression", kernel = "radial", cost = 1)
  
  # Return the list of fitted models
  list(
    lm_model = lm_fit,
    rf_model = rf_fit,
    xgb_model = xgb_fit,
    svm_model = svm_fit
  )
}
