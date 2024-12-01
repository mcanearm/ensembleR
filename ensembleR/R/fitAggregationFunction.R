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
fitAggregationFunction <- function(y_hat, Y, method="EM", ...) {
    if (method == "EM") {
        fitAggregationFunction_EM(y_hat, Y, ...)
    } else if (method == "optim") {
        fitAggregationFunction_optim(y_hat, Y, ...)
    } else {
        stop("Method must be either 'EM' or 'optim'")
    }
}


#' @title Fit Aggregation function by simple LM fit
#' @describeIn fitAggregationFunction Fit a standard linear model to the data for aggregation.
fitAggregationFunction_optim <- function(y_hat, Y, ...) {
    # TODO: Instead of calibrating based on a single validation set, use cross validation
    # to estimate the calibration parameters around:
    # 1. The standard deviation estimates
    # 2. A final "stddev discount" parameter that optimizes for the coverage requested.
    agg_lm <- lm(Y ~ y_hat, data.frame(Y, y_hat))
    sigma <- sd(agg_lm$residuals)

    calibrator <- ensembleR::fitCalibrator(Y, agg_lm$fitted.values, y_hat)
    # TODO: consider modifying the bias term and/or fitting the residuals directly
    structure(
        list(model=agg_lm, calibrator=calibrator, sigma=sigma),
        class = c("ModelAggregator", "list")
    )

}

#' @title Log Likelihood function
#'
#' @description Log likelihood method for both numerical optimization and EM
#' algorithm approaches to finding the optimal mixture.
ll_partial <- function(Y, y_hat) {
    ll_func <- function(beta_vect) {
        y_hat_mu <- y_hat %*% beta_vect
        sigma <- sqrt(mean((Y - y_hat_mu)^2))
        -sum(dnorm(Y, mean = y_hat_mu, sd = sigma, log = TRUE))
    }
    ll_func
}


#' @title Fit Aggregation Function EM
#' @export
#' @describeIn fitAggregationFunction Fit a Normal mixture model on the regressors using the EM algorithm
fitAggregationFunction_EM <- function(y_hat, Y, tol = 1e-6, max_iter = 1000, verbose = FALSE) {
    # Number of predictors
    k <- ncol(y_hat)

    # Initialize weights (betas) uniformly
    betas <- rep(1 / k, k)

    # Initialize parameters
    sigma <- sqrt(mean((Y - y_hat %*% betas)^2))
    iter <- 0
    diff <- Inf

    while (iter < max_iter && diff > tol) {
        # E-step: Compute responsibilities
        y_hat_mu <- y_hat %*% betas
        likelihoods <- sapply(1:k, function(j) {
            beta_temp <- rep(0, k)
            beta_temp[j] <- 1
            dnorm(Y, mean = y_hat %*% beta_temp, sd = sigma, log = TRUE)
        })
        responsibilities <- exp(likelihoods - rowMeans(likelihoods))
        responsibilities <- responsibilities / rowSums(responsibilities)

        # M-step: Update betas
        betas_new <- colMeans(responsibilities)
        sigma_new <- sqrt(mean((Y - y_hat %*% betas_new)^2))

        # Check for convergence
        diff <- sum(abs(betas - betas_new))
        betas <- betas_new
        sigma <- sigma_new
        iter <- iter + 1

        if (verbose) {
            cat(sprintf("Iteration: %d, Log Likelihood: %.4f, Diff: %.6f\n", iter, -sum(likelihoods), diff))
        }
    }

    if (iter == max_iter) {
        warning("EM algorithm did not converge within the maximum number of iterations.")
    }

    # Finalize and return the results
    structure(
        list(
            betas = betas,
            sigma = sigma,
            iterations = iter,
            log_likelihood = -sum(likelihoods)
        ),
        class = c("AggregationFunction", "list")
    )
}

#' @export
predict.ModelAggregator <- function(obj, y_hat, alpha=0.05, n_trials=1000, ...) {
    mus <- predict(obj$model, y_hat)
    pred_sd <- predict(obj$calibrator, data.frame(y_hat))
    pred_sd <- ifelse(pred_sd < 0, obj$sigma, pred_sd)
    predicted_dists <- matrix(
        rnorm(n_trials*nrow(y_hat), mean = mus, sd = pred_sd),
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
