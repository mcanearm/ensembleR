#' @title Fit a Normal mixture model on the data
#' @export
#' @description High level general interface for fitting aggregations of regressor
#' variables into a single output prediction. The individual logic of aggregation
#' methods and their subsequent application to new data is handled by individual
#' fitting methods. S3 dispatch is used to determine the correct method
#' of prediction.
#'
#' To add a new method, one should create a new file and define both the fitting
#' method and the prediction method. The fit method should output a unique S3 class
#' name, and by convention it should be `ModelAggregator_{method}`.
#' @param Y The true Y values for fitting
#' @param y_hat The predictions output from a model. Note that this is passed
#' as an \eqn{N \times K} matrix, where \eqn{K} is the number of predictors.
#' @param ... Additional arguments to pass to the individual aggregation fitting methods. Each has their own
#' set of parameters for customizing the aggregation process according to the methodology
#' in the model.
#' @returns A model aggregation object that contains metadata required for
#' predicting new values from a provided set of predictions.
#' @examples
#' \dontrun{
#' # Make three small test models for mixing
#' y_hat1 <- rnorm(1000, 3, 1)
#' y_hat2 <- rnorm(1000, 3, 1.5)
#' y_hat3 <- rnorm(1000, 3, 0.5)
#'
#' y_hat <- cbind(y_hat1, y_hat2, y_hat3)
#'
#' w <- c(0.5, 0.2, 0.3)
#' y <- (y_hat %*% w + rnorm(1000))[, 1]
#'
#' model_aggregator <- fitAggregationFunction(y, y_hat, method="quantile")
#' predict(model_aggregator, y_hat, alpha=0.05)
#' }
fitAggregationFunction <- function(Y, y_hat, method="EM", ...) {
    if (method == "EM") {
        fitAggregationFunction_EM(Y, y_hat, ...)
    } else if (method == "LM") {
        fitAggregationFunction_lm(Y, y_hat, ...)
    } else if (method == "bootLM") {
        fitAggregationFunction_bootLM(Y, y_hat, ...)
    } else if (method == "quantile") {
        fitAggregationFunction_quantile(Y, y_hat, ...)
    } else {
        stop("Method must one of 'EM', 'LM', 'bootLM', 'quantile'")
    }
}

