#' @title Fit models
#' @export
#' @description` Fit all the models + the aggregation function
fit_models <- function(X, Y, ...) {
    # Split the data into training and validation sets
    set.seed(42)
    train_idx <- sample(1:nrow(X), 0.8 * nrow(X))
    train_X <- X[train_idx, ]
    val_X <- X[-train_idx, ]
    train_Y <- Y[train_idx]
    val_Y <- Y[-train_idx]

    # 1. Linear Regression Model
    lm_fit <- lm(train_Y ~ ., data = data.frame(train_X, train_Y))
    lm_pred <- predict(lm_fit, newdata = data.frame(val_X))

    # 2. Random Forest Model
    rf_fit <- ranger::ranger(train_Y ~ ., data = data.frame(train_X, train_Y), num.trees = 256)
    rf_pred <- predict(rf_fit, data.frame(val_X))$predictions

    # 3. XGBoost Model
    xgb_train <- xgboost::xgb.DMatrix(data = as.matrix(train_X), label = train_Y)
    xgb_fit <- xgboost::xgboost(data = xgb_train, objective = "reg:squarederror", nrounds = 500, verbose = 0)
    xgb_pred <- predict(xgb_fit, newdata = as.matrix(val_X))

    # 4. SVM Regression Model
    svm_fit <- svm(x = as.matrix(train_X), y = train_Y, type = "eps-regression", kernel = "radial", cost = 1)
    svm_pred <- predict(svm_fit, newdata = as.matrix(val_X))

    # Combine predictions from all models
    y_hat <- cbind(lm_pred, rf_pred, xgb_pred, svm_pred)

    # Use EM algorithm to find the optimal weights
    aggregation_function <- fitAggregationFunction(y_hat, val_Y, ...)

    # Return all fitted models and the aggregation function
    list(
        lm_model = lm_fit,
        rf_model = rf_fit,
        xgb_model = xgb_fit,
        svm_model = svm_fit,
        aggregation_function = aggregation_function
    )
}


#' @exportS3Method ensembleR::plot
plot.ModelEnsemble <- function(obj, ...) {
    # Plot the models

    # DECIDE HOW TO PLOT - mix of histograms?
    hist(rnorm(100))
}

#' @exportS3Method ensembleR::summary
summary.ModelEnsemble <- function(obj, ...) {
    # Summarize the models
    print("Model Ensemble Summary")
    print(obj)
}
