#' @title Fit models
#' @export
#' @describeIn Fit the specified models
#' TODO: ADD MORE DOCUMENTATION HERE
fit_models <- function(X, y, ...) {
    # Fit the models
    rf_model <- fit_rf(X, y, ...)
    svm_model <- fit_svm(X, y, ...)
    xgb_model <- fit_xgb(X, y, ...)

    all_models <- list(rf_model = rf_model, svm_model = svm_model, xgb_model = xgb_model)
    class(all_models) <- c("ModelEnsemble", class(all_models))

    all_models
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
