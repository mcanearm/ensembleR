#' @title Fit Aggregation Function EM
#' @export
#' @description Fit a Normal mixture model on the regressors using the EM algorithm
#' @param tol Minimum distance between successive likelihoods before the algorithm is
#' considered to have converged
#' @param max_iter Maximum number of iterations the EM algorithm is allowed to run
#' @param verbose Logical indicator for whether or not to print progress messages
#' @inheritParams fitAggregationFunction
#' @inherit fitAggregationFunction_bootLM note
fitAggregationFunction_EM <- function(Y, y_hat, tol = 1e-4, max_iter = 1000, verbose = FALSE, ...) {
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
#' @describeIn fitAggregationFunction_EM Predict method for the S3 EM Model Aggregation function.
#' @param n_trials The number of random draws to run for the mixture
#' @param alpha The significance level for the prediction interval
#' @param object The fitted ModelAggregator_EM function
#' @param ... Additional arguments; unused and added to align with predict interface.
predict.ModelAggregator_EM <- function(object, y_hat, alpha=0.05, n_trials=1000, ...) {
    w <- object$betas
    sigma <- object$sigma

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
        'sd' = pred_sd
    )
}
