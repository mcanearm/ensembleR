#' @title Fit models
#' @export
#' @description This wrapper function fits all models and returns the fitted
#' model objects. Note that this includes four models - SVM, Random Forest,
#' XGBoost, and a simple OLS model. For more information on these models,
#' see \link[ranger]{ranger}, \link[e1071]{svm}, \link[xgboost]{xgboost}, and
#' \link[stats]{lm}.
#' @param X A dataframe of covariates/explanatory variables. Strictly speaking,
#' it can be a matrix, but it is assumed to be a dataframe so that categorical
#' variables are handled effectively.
#' @param Y A numeric vector for the response
#' @param aggregation_method A string indicating the method to use for
#' aggregation.
#' @param validation_pct The percentage of the training data to hold apart
#' for usage in fitting the aggregation method.
#' @import ranger e1071 xgboost
fit_models <- function(X, Y, aggregation_method=NULL, validation_pct=0.2, ...) {
    # Split the data into training and validation sets
    train_idx <- sample(1:nrow(X), (1-validation_pct) * nrow(X))
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
        fit_features <- as.matrix(train_X)
        val_features <- as.matrix(val_X)
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
    if (!is.null(aggregation_method)) {
        aggregation_function <- ensembleR::fitAggregationFunction(val_Y, y_hat, method=aggregation_method, ...)
        aggregated_predictions <- predict(aggregation_function, y_hat)
    } else {
        aggregated_predictions <- NULL
        aggregation_function <- NULL
    }

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

#' @title Prediction method for ModelEnsemble
#' @export
#' @describeIn fit_models S3 Prediction method for the ModelEnsemble object.
#' @param object A fitted ModelEnsemble object
#' @param return_components A logical indicating whether to return the individual
#' method predictions alongside the aggregation. Only relevant if the
#' aggregation function is fitted. If it is not fitted, then this argument
#' has no effect.
#' @param ... Arbitrary arguments to be passed to the fitted aggregation
#' function. These arguments do not apply and are unused if the aggregation
#' function is not fitted.
#' @return If no aggregation method is present in the `object`, a dataframe
#' is returned containined the output all four methods. If an aggregation
#' method is fitted, the results of the aggregation are returned, namely a
#' dataframe consisting of the mean, lower and upper bounds of a prediction
#' interval, and the predicted standard deviation for a particular value. See
#' \link[ensembleR]{fitAggregationFunction} for more details.
predict.ModelEnsemble <- function(object, X, return_components=FALSE, ...) {
    # Predict the ensemble
    if (!is.null(object$char_encoder)) {
        pred_features <- predict(object$char_encoder, X)
    } else {
        pred_features <- as.matrix(X)
    }

    # Make predictions for the four models
    lm_pred <- predict(object$lm_model, data.frame(X))
    rf_pred <- predict(object$rf_model, data.frame(X))$predictions
    xgb_pred <- predict(object$xgb_model, xgboost::xgb.DMatrix(data = as.matrix(pred_features)))
    svm_pred <- predict(object$svm_model, as.matrix(pred_features))

    # Combine predictions
    y_hat <- cbind(lm_pred, rf_pred, xgb_pred, svm_pred)

    # Predict the ensemble
    if (!is.null(object$aggregation_function)) {
        aggregation <- predict(object$aggregation_function, y_hat, ...)
    } else {
        aggregation <- NULL
    }

    if (return_components && !is.null(aggregation)) {
        list(
            "aggregated"=aggregation,
            "y_hat" = y_hat
        )
    } else if (is.null(aggregation)) {
        y_hat
    } else {
        aggregation
    }
}
