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
fitAggregationFunction <- function(y_hat, Y, ...) {
    # Compile and run the STAN model

    cov_mat <- cov(Y - y_hat)
    nll <- function(beta_vect) {
        betas <- c(beta_vect, 1 - sum(beta_vect))
        y_hat_mu <- y_hat %*% betas
        sigma <- sqrt(betas %*% cov_mat %*% betas)
        -sum(dnorm(Y, y_hat_mu, sigma, log = TRUE))
    }

    initial <- rep(1 / ncol(y_hat), ncol(y_hat) - 1)
    optimal_beta <- optim(
        initial,
        nll,
        method = "L-BFGS-B",
        lower = rep(0, ncol(y_hat)),
        upper = rep(1, ncol(y_hat)),
        ...
    )
    beta <- c(optimal_beta$par, 1 - sum(optimal_beta$par))
    sigma <- sqrt(beta %*% cov_mat %*% beta)[1]

    calibrator <- ensembleR::fitCalibrator(Y, y_hat %*% beta, y_hat)

    # TODO: consider modifying the bias term and/or fitting the residuals directly

    structure(
        list(optim_results=optimal_beta, sigma=sigma, beta=beta, calibrator=calibrator),
        class = c("ModelAggregator", "list")
    )

}


#' @export
predict.ModelAggregator <- function(obj, y_hat, alpha=0.05, n_trials=1000, ...) {
    mus <- y_hat %*% obj$beta
    pred_sd <- predict(obj$calibrator, data.frame(y_hat))
    pred_sd <- ifelse(pred_sd < 0, obj$sigma, pred_sd)
    predicted_dists <- matrix(
        rnorm(1000*nrow(y_hat), mean = mus, sd = pred_sd),
        nrow = nrow(y_hat),
        byrow = FALSE
    )
    pi <- t(apply(predicted_dists, 1, function(preds) {
        quantile(preds, probs = c(alpha/2, 1 - alpha/2))
    }))
    cbind(
        'mean' = rowMeans(predicted_dists),
        'lower' = pi[, 1],
        'upper' = pi[, 2],
        'pred_sd' = pred_sd
    )
}
