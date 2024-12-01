#' @title Fit models
#' @export
#' @description` Fit all the models + the aggregation function
fit_models <- function(X, Y, ...) {
    # Split the data into training and validation sets
    train_idx <- sample(1:nrow(X), 0.8 * nrow(X))
    train_X <- X[train_idx, ]
    val_X <- X[-train_idx, ]
    train_Y <- Y[train_idx]
    val_Y <- Y[-train_idx]

    # handle any categorical variables if they are present for the matrix variables
    char_encoder <- ensembleR::fit_char_encoder(train_X)
    if (!is.null(char_encoder)) {
        fit_features <- predict(char_encoder, train_X)
        val_features <- predict(char_encoder, val_X)
    } else {
        fit_features <- as.matrix(X)
    }

    # 1. Linear Regression Model
    # Note that this method assumes a dataframe interface
    lm_fit <- lm(train_Y ~ ., data = data.frame(train_X, train_Y))
    lm_pred <- predict(lm_fit, data.frame(val_X))

    # 2. Random Forest Model - the ranger package also simply utilizes the dataframe
    # interface, so automatically handles categorical variables
    rf_fit <- ranger::ranger(train_Y ~ ., data = data.frame(train_X, train_Y), num.trees = 256)
    rf_pred <- predict(rf_fit, data.frame(val_X))$predictions

    # 3. XGBoost Model - requires the fit_features matrix
    xgb_train <- xgboost::xgb.DMatrix(data = fit_features, label = train_Y)
    xgb_fit <- xgboost::xgboost(data = xgb_train, objective = "reg:squarederror", nrounds = 500, verbose = 0)
    xgb_pred <- predict(xgb_fit, val_features)

    # 4. SVM Regression Model
    svm_fit <- e1071::svm(x = fit_features, y = train_Y, type = "eps-regression", kernel = "radial", cost = 1)
    svm_pred <- predict(svm_fit, val_features)

    # Combine predictions from all models
    y_hat <- cbind(lm_pred, rf_pred, xgb_pred, svm_pred)

    # Use EM algorithm to find the optimal weights
    aggregation_function <- ensembleR::fitAggregationFunction(y_hat, val_Y, ...)

    # Return all fitted models and the aggregation function
    structure(list(
        lm_model = lm_fit,
        rf_model = rf_fit,
        xgb_model = xgb_fit,
        svm_model = svm_fit,
        char_encoder = char_encoder,
        aggregation_function = aggregation_function,
        y_hat=y_hat,
        val_Y=val_Y
    ), class="ModelEnsemble")
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
