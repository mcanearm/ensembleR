#' @title Random Forest
#' @description Fit a random forest model using the ranger package
fit_rf <- function(X, y, ...) {
    # Do the model fitting for an arbitrary set of y and X covariates
    rf_model <- ranger::ranger(mpg ~ ., data = cars, num.trees = 500, mtry = 2, importance = 'impurity')
    rf_model
}


#' @title SVM
#' @description
#' Fit a small SVM on arbitrary inputs:w
fit_svm <- function(X, y, ...) {
    svm_model <- e1071::svm(mpg ~ ., data = cars, kernel = "radial")
}


fit_xgb <- function(X, y, ...) {
    # chatGPT generated
    xgb_model <- xgboost::xgboost(data = train_data, label = train_label, max_depth = 3, eta = 0.1, nrounds = 50, objective = "multi:softmax", num_class = 3)
    xgb_model
}


