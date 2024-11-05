# #' @title Fit a Normal mixture model on the data using STAN
# #' @export
# fitSTANModel <- function(Y, y_preds,...) {
#     # Compile and run the STAN model
#     stan_model <- system.file("stan_programs", "normal_mixture.stan", package = "ensembleR")

#     data_list <- list(y_true=Y, f_pred=y_preds, N = length(Y), K = ncol(Y))
#     # Compile the STAN model
#     sm <- rstan::stan_model(stan_model)

    
#     # Sample from the posterior
#     fit <- rstan::sampling(sm, data = data_list, iter = 2000, chains = 4)

#     # compiled_model <- rstan::stan(stan_model, data = data_list, iter = 2000, chains = 4)

# }

# y_vals <- matrix(rnorm(3000, 0, 2), ncol=3)
# true_y <- rnorm(1000, 0, 1)

# fitSTANModel(true_y, y_vals)

library(rstan)


y_hat1 <- rnorm(100, 2, 1)
y_hat2 <- rnorm(100, 5, 2)

w1 <- 0.3
w2 <- 0.7
alpha <- 3

y <- alpha + w1 * y_hat1  + w2 * y_hat2 + rnorm(100)
y_hat <- cbind(y_hat1, y_hat2)


model_file <- "/Users/mcanearm/Projects/ensembleR/ensembleR/inst/stan_programs/normal_mixture.stan"


stan_fit <- stan(file=model_file, data=list(y=y, N=nrow(y_hat), K=ncol(y_hat), x=y_hat))

all_samples <- as.matrix(stan_fit)

traceplot(stan_fit)
hist(all_samples[, 1])
