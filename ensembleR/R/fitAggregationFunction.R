#' @title Fit a Normal mixture model on the data using STAN
#' @export
#' @description Fit a STAN model to the predicted outputs in order to aggregate the final predictions.
#' Currently, this uses a small mixture model that assumes (1) simplex weights on each model and (2) constant variance across all models.
#' Then, return a function that aggregates an unessen set of predictions.
#' @param Y The true Y values for fitting
#' @param y_hat The predictions output from a model. Note that this is passed as an $NxK$ matrix, where $K$ is the number of predictors.
#' @param ... Additional arguments to pass to the rstan::sampling function
#' @importFrom rstan stan_model sampling
#' @examples
#' \dontrun{
#'     # Make three small test models for mixing
#'     y_hat1 <- rnorm(1000, 3, 1)
#'     y_hat2 <- rnorm(1000, 3, 1.5)
#'     y_hat3 <- rnorm(1000, 3, 0.5)
#'
#'     y_hat <- cbind(y_hat1, y_hat2, y_hat3)
#'     w <- c(0.5, 0.2, 0.3)
#'     y <- (y_hat %*% w + rnorm(1000))[, 1]
#'
#'     stan_fit <- fitSTANModel(y, y_hat)
#'     all_samples <- as.matrix(stan_fit)
#'     rstan::traceplot(stan_fit)
#'     hist(all_samples[, 1])
#' }
fitAggregationFunction <- function(y_hat, Y, ...) {
    # Compile and run the STAN model
    # stan_model <- system.file("stan_programs", "normal_mixture.stan", package = "ensembleR")
    stan_model <- "~/Projects/ensembleR/ensembleR/inst/stan_programs/normal_mixture.stan"

    data_list <- list(
        y = Y,
        y_hat = y_hat,
        N = length(Y),
        K = ncol(y_hat)
    )

    # Compile the STAN model
    sm <- rstan::stan_model(stan_model)

    # Sample from the posterior
    fit <- rstan::sampling(sm, data = data_list, cores = 4)

    # get only the simulations for the beta and sigma draws
    sims <- rstan::extract(fit, pars = c("beta", "sigma"))
    sd_ests <- apply(Y - y_hat, 2, sd)

    aggregate_fn <- function(pred_matrix, alpha = 0.025) {
        mix_choice <- apply(sims$beta, 1, function(probs) {
            sample(1:length(probs), 1, prob = probs)
        })
        predicted_dists <- apply(pred_matrix, 1, function(y_hat_row) {
            mu_val <- y_hat_row[mix_choice]
            sd_vals <- sd_ests[mix_choice]
            rnorm(length(mix_choice),
                  mean = mu_val,
                  sd = sd_vals)
        })

        pi <- t(apply(predicted_dists, 2, function(preds) {
            quantile(preds, probs = c(0.025, 0.975))
        }))
        cbind(
            'mean' = colMeans(predicted_dists),
            'lower' = pi[, 1],
            'upper' = pi[, 2]
        )
    }
    aggregate_fn
}
