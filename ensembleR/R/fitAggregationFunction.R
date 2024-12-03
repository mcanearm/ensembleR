#' @title Fit a Normal mixture model on the data
#' @export
#' @description Fit a normal mixture that maximizes the log likelihood of the of the weights of
#' each method. This uses the base R `optim` function to find the optimal weights
#' for each method.
#' @param Y The true Y values for fitting
#' @param y_hat The predictions output from a model. Note that this is passed as an $NxK$ matrix, where $K$ is the number of predictors.
#' @param ... Additional arguments to pass to the optim function
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
#' model_aggregator <- fitAggregationFunction(y, y_hat)
#' predict(model_aggregator, y_hat, alpha=0.05)
#' plot(fitAggregationFunction)
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
    }else {
        stop("Method must one of 'EM', 'LM', 'bootLM', 'quantile'")
    }
}


