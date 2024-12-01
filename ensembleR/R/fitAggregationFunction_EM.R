#' @title Fit a Normal mixture model on the data using EM algorithm
#' @describeIn fitAggregationFunction Fit a Normal mixture model on the data using the EM algorithm
fitAggregationFunction_EM <- function(y_hat, Y, tol = 1e-6, max_iter = 1000, verbose = FALSE) {
  # Number of predictors
  k <- ncol(y_hat)
  
  # Initialize weights (betas) uniformly
  betas <- rep(1 / k, k)
  
  # Initialize parameters
  sigma <- sqrt(mean((Y - y_hat %*% betas)^2))
  iter <- 0
  diff <- Inf
  log_likelihood_history <- c()
  
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
    
    # Compute log likelihood for current iteration
    current_log_likelihood <- -sum(dnorm(Y, mean = y_hat_mu, sd = sigma_new, log = TRUE))
    log_likelihood_history <- c(log_likelihood_history, current_log_likelihood)
    
    # Check for convergence
    diff <- sum(abs(betas - betas_new))
    betas <- betas_new
    sigma <- sigma_new
    iter <- iter + 1
    
    if (verbose) {
      cat(sprintf("Iteration: %d, Log Likelihood: %.4f, Diff: %.6f\n", iter, current_log_likelihood, diff))
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
      log_likelihood = log_likelihood_history,
      iterations = iter
    ),
    class = c("ModelAggregator", "list")
  )
}

#' @title Main Aggregation Function
#' @describeIn fitAggregationFunction Add EM method and integrate it with main function
fitAggregationFunction <- function(y_hat, Y, method = "EM", tol = 1e-6, max_iter = 1000, ...) {
  if (method == "EM") {
    fitAggregationFunction_EM(y_hat, Y, tol = tol, max_iter = max_iter, ...)
  } else if (method == "optim") {
    fitAggregationFunction_optim(y_hat, Y, ...)
  } else {
    stop("Method must be either 'EM' or 'optim'")
  }
}
