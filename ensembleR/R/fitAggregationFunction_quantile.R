#' @title Fit Aggregation by Quantile Regression
#' @export
#' @describeIn fitAggregationFunction Fit a quantile regression model to the data for aggregation.
#' @import quantreg
fitAggregationFunction_quantile <- function(Y, y_hat, quantiles = c(0.025, 0.5, 0.975), ...) {
    # Ensure y_hat is a matrix
    y_hat <- as.matrix(y_hat)

    # Fit quantile regression for each specified quantile
    models <- lapply(quantiles, function(q) {
        quantreg::rq(Y ~ y_hat, tau = q, ...)
    })

    # Extract coefficients for each quantile
    coefficients <- lapply(models, coef)

    # Return the fitted models and coefficients
    structure(
        list(
            models = models,
            coefficients = coefficients,
            quantiles = quantiles
        ),
        class = c("QuantileAggregation", "list")
    )
}

#' @export
#' @describeIn fitAggregationFunction General S3 method for the quantile prediction method
predict.QuantileAggregation <- function(obj, y_hat, ...) {
    y_hat <- as.matrix(y_hat)

    # Generate predictions for each quantile
    predictions <- sapply(obj$models, function(model) {
        cbind(1, y_hat) %*% coef(model)  # Add intercept and multiply by coefficients
    })

    # by empirical rule, 95% of values are within two standard deviations.
    range <- (predictions[, 3] - predictions[, 1])/2

    # align this method with the output of the other methods
    out <- cbind(predictions[, c(2, 1, 3)], range)
    colnames(out) <- c("mean", "lower", "upper", "pred_sd")
    out
}
