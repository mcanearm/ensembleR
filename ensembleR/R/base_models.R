#' @title Random Forest
#' @description Fit a random forest model using the ranger package
#' @import ranger
#' @param ... Additional fitting arguments to be passed to ranger function
#' @param X Covariates for fitting
#' @param y target variable
fit_rf <- function(X, y, ...) {
    # Do the model fitting for an arbitrary set of y and X covariates

    # assumed that all preprocessing is done prior to this step
    fit_df <- cbind.data.frame(y, X)
    rf_model <- ranger::ranger(y ~ ., data = fit_df, ...)
    rf_model
}


#' @title SVM
#' @description
#' Fit a small SVM on arbitrary inputs
fit_svm <- function(X, y, ...) {
    fit_df <- cbind.data.frame(y, X)
    svm_model <- e1071::svm(y ~ ., data = fit_df, ...)
    svm_model
}


#' @title XGB
fit_xgb <- function(X, y, ...) {
    fit_df <- cbind.data.frame(y, X)
    xgb_model <- xgboost::xgboost(data = as.matrix(X), label=y, objective="reg:squarederror", nrounds=1000)
    xgb_model
}
