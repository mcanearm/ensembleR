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
    is_char <- apply(X, 2, function(col) {
        tmp <- suppressWarnings(as.numeric(col))
        all(is.na(tmp))
    })
    char_cols <- colnames(X)[is_char]
    numeric_cols <- setdiff(colnames(X), char_cols)
    char_formula <- formula(paste0(" ~ ", paste0(char_cols, collapse = " + ")))
    dummy_mod <- caret::dummyVars(char_formula, data = X, fullRank = TRUE)

    dummy_vars <- predict(dummy_mod, newdata = X)
    fit_features <- as.matrix(cbind(X[, numeric_cols], dummy_vars))

    xgb_model <- xgboost::xgboost(data = fit_features, label=y, objective="reg:squarederror", nrounds=250, ...)
    xgb_model
}
