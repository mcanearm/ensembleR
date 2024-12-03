library(quantreg)

fitAggregationFunction_quantile <- function(y_hat, Y, quantiles = c(0.05, 0.5, 0.95), ...) {
  # Ensure y_hat is a matrix
  y_hat <- as.matrix(y_hat)
  
  # Fit quantile regression for each specified quantile
  models <- lapply(quantiles, function(q) {
    rq(Y ~ y_hat, tau = q, ...)
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

predict.QuantileAggregation <- function(obj, y_hat, ...) {
  y_hat <- as.matrix(y_hat)
  
  # Generate predictions for each quantile
  predictions <- sapply(obj$models, function(model) {
    cbind(1, y_hat) %*% coef(model)  # Add intercept and multiply by coefficients
  })
  
  # Return quantile predictions as a data frame
  colnames(predictions) <- paste0("Q", obj$quantiles * 100)
  as.data.frame(predictions)
}
