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
    } else if (method == "lm") {
        fitAggregationFunction_lm(Y, y_hat, ...)
    } else {
        stop("Method must be either 'EM' or 'lm'")
    }
}


#' @title Fit Aggregation function by simple LM fit
#' @describeIn fitAggregationFunction Fit a standard linear model to the data for aggregation.
fitAggregationFunction_lm <- function(Y, y_hat, ...) {
    # TODO: Instead of calibrating based on a single validation set, use cross validation
    # to estimate the calibration parameters around:
    # 1. The standard deviation estimates
    # 2. A final "stddev discount" parameter that optimizes for the coverage requested.
    y_hat_names <- colnames(y_hat)  # required for using the predict method later with LM...
    agg_lm <- lm(Y ~ y_hat, data.frame(Y, y_hat))
    sigma <- sd(agg_lm$residuals)

    # calibrator <- ensembleR::fitCalibrator(Y, agg_lm$fitted.values, y_hat)
    # TODO: consider modifying the bias term and/or fitting the residuals directly
    structure(
        list(model=agg_lm, sigma=sigma, prediction_names=y_hat_names),
        class = c("ModelAggregator_lm", "list")
    )
}

#' @export
predict.ModelAggregator_lm <- function(obj, y_hat, alpha=0.05, n_trials=1000, ...) {
    colnames(y_hat) <- obj$prediction_names
    mus <- predict(obj$model, data.frame(y_hat))
    # pred_sd <- predict(obj$calibrator, data.frame(y_hat))
    # pred_sd <- ifelse(pred_sd < 0, obj$sigma, pred_sd)
    pred_sd <- obj$sigma
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



#' @title Fit Aggregation Function EM
#' @export
#' @describeIn fitAggregationFunction Fit a Normal mixture model on the regressors using the EM algorithm
fitAggregationFunction_EM <- function(Y, y_hat, tol = 1e-4, max_iter = 1000, verbose = FALSE) {
    # Number of predictors
    k <- ncol(y_hat)

    # Initialize weights (betas) uniformly
    betas <- runif(k)
    betas <- betas / sum(betas)

    # Initialize separate sigmas for each method
    sigma <- apply(Y - y_hat, 2, sd)
    iter <- 0
    diff <- Inf
    loglik_old <- -Inf

    while (iter < max_iter && diff > tol) {
        # E-step: Compute responsibilities
        log_prob_mat <- sapply(1:k, function(j) {
            dnorm(Y, mean = y_hat[, j], sd = sigma[j], log = TRUE) + log(betas[j])
        })
        max_log_prob <- apply(log_prob_mat, 1, max)
        prob_mat <- exp(log_prob_mat - max_log_prob)
        sum_prob <- rowSums(prob_mat)
        prob_mat <- prob_mat / sum_prob
        loglik <- sum(max_log_prob + log(sum_prob))  # Update log likelihood

        # M-step: Update betas and sigmas
        betas_new <- colSums(prob_mat) / nrow(y_hat)
        sigma_new <- sqrt(colSums((Y - y_hat)^2 * prob_mat) / colSums(prob_mat))

        # Check for convergence
        diff <- abs(loglik - loglik_old)
        loglik_old <- loglik
        betas <- betas_new
        sigma <- sigma_new
        iter <- iter + 1

        if (verbose) {
            cat(sprintf("Iteration: %d, Log Likelihood: %.4f, Diff: %.6f\n", iter, loglik, diff))
        }
    }

    if (iter == max_iter) {
        warning("EM algorithm did not converge within the maximum number of iterations.")
    }

    # Return results
    structure(
        list(
            betas = betas,
            sigma = sigma,
            iterations = iter,
            log_likelihood = loglik
        ),
        class = c("ModelAggregator_EM", "list")
    )
}


#' @export
predict.ModelAggregator_EM <- function(obj, y_hat, alpha=0.05, n_trials=1000, ...) {
    w <- obj$betas
    sigma <- obj$sigma

    # assume the same sample weights and distribution for each variable
    choices <- sample(1:length(w), n_trials, replace=TRUE, prob=w)
    predicted_dists <- matrix(
        rnorm(nrow(y_hat)*n_trials, mean=y_hat[, choices], sd=sigma[choices]),
        byrow=FALSE,
        nrow=nrow(y_hat)
    )
    pi <- t(apply(predicted_dists, 1, function(preds) {
        quantile(preds, probs = c(alpha/2, 1 - alpha/2))
    }))
    pred_sd <- apply(predicted_dists, 1, sd)
    cbind(
        'mean' = rowMeans(predicted_dists),
        'lower' = pi[, 1],
        'upper' = pi[, 2],
        'pred_sd' = pred_sd
    )
}

