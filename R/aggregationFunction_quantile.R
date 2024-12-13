#' @title Fit Aggregation by Quantile Regression
#' @export
#' @description Function Fit a quantile regression model to the data for aggregation.
#' @inheritParams fitAggregationFunction
#' @inherit fitAggregationFunction_bootLM note
#' @import quantreg
fitAggregationFunction_quantile <- function(Y, y_hat, ...) {
    # Ensure y_hat is a matrix
    y_hat <- as.matrix(y_hat)

    # statically code right now to a 95% prediction interval
    quantiles = c(0.025, 0.5, 0.975)

    # TODO: @haowen the quantreg method has an easy way to fit multiple quantiles
    # at once - could you modify this code to fit on a range of tau values
    # and then use the alpha value to find the closest prediction interval
    # for that alpha value?

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
#' @describeIn fitAggregationFunction_quantile General S3 method for the quantile prediction method
#' @inheritParams fitAggregationFunction
#' @inheritParams fitAggregationFunction_EM
#' @param alpha The significance level for the prediction interval. Note that this parameter is
#' hardcoded at 0.05 for now. A warning is raised if another alpha value is provided.
predict.QuantileAggregation <- function(object, y_hat, alpha=0.05, ...) {
    y_hat <- as.matrix(y_hat)

    if (alpha != 0.05) {
        warning("Currently, only an alpha value of 0.05 is supported. Ignoring alpha value.")
    }

    # Generate predictions for each quantile
    predictions <- sapply(object$models, function(model) {
        cbind(1, y_hat) %*% coef(model)  # Add intercept and multiply by coefficients
    })

    # by empirical rule, 95% of values are within two standard deviations.
    range <- (predictions[, 3] - predictions[, 1])/2

    # align this method with the output of the other methods
    out <- cbind(predictions[, c(2, 1, 3)], range)
    colnames(out) <- c("mean", "lower", "upper", "pred_sd")
    out
}
