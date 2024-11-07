#' @title Fit a Normal mixture model on the data using STAN
#' @export
#' @description Fit a STAN model to the predicted outputs in order to aggregate the final predictions.
#' Currently, this uses a small mixture model that assumes (1) simplex weights on each model and (2) constant variance across all models.
#' TODO: this NEEDS to do something other than assuming constant variance, but I'm unsure what.
#' @param Y The true Y values for fitting
#' @param y_hat The predictions output from a model. Note that this is passed an NxK matrix
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
fitSTANModel <- function(y_hat, Y, ...) {
    # Compile and run the STAN model
    stan_model <- system.file("stan_programs", "normal_mixture.stan", package = "ensembleR")

    data_list <- list(
        y = Y,
        y_hat = y_hat,
        N = length(Y),
        K = ncol(y_hat)
    )

    # Compile the STAN model
    sm <- rstan::stan_model(stan_model)

    # Sample from the posterior
    fit <- rstan::sampling(sm, data = data_list, ...)
    fit
}


#' @title Predict using the STAN model
#' @description Runs predictions for a given set of y_hat. Note that the
#' dimensionality must be the same as in the original training data.
#' @exportS3Method ensembleR::predict
predict.fitSTANModel <- function(y_hat) {
    NULL
}

#' @title STAN model traceplot
#' @exportS3Method ensembleR::plot
#' @description
#' Plot the traceplot fo the fitSTANModel output. Accepts the same options.
#'
plot.fitSTANModel <- function(x, ...) {
    rstan::traceplot(x, ...)
}
