#' @title Fit a Normal mixture model on the data using STAN
#' @export
fitSTANModel <- function(Y, ...) {
    # Compile and run the STAN model
    stan_model <- stan(model_code = stan_model_code, data = data_list, iter = 2000, chains = 4)
}
