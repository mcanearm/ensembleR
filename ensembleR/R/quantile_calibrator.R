install.packages("quantreg")
install.packages("SparseM")
library(quantreg)
fitQuantileBounds <- function(Y, y_hat, lower_quantile = 0.025, upper_quantile = 0.975) {
   # Fit lower bound
  lower_model <- quantreg::rq(Y ~ y_hat, tau = lower_quantile)
  
  # Fit upper bound
  upper_model <- quantreg::rq(Y ~ y_hat, tau = upper_quantile)
  
  list(
    lower_bound_model = lower_model,
    upper_bound_model = upper_model
  )
}
