/*
Generate a simple normal mixture model. This is fit like a linear regression,
with the exception that the betas in this case are a "simplex", which in
STAN means that the weights all add up to 1.

We also allow very general priors here, but these could be changed somehow.
TODO:
    1) Investigate better ways to do the mixture
    2) How can we take into account covariance between predictions (predictions that miss more often should indicate missed in the other models too!)
*/
data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] y_hat;
  vector[N] y;
}
parameters {
  simplex[K] beta;
  real<lower=0> sigma;
}
model {
    vector[N] y_pred;
    for (i in 1:N) {
        y_pred[i] = dot_product(y_hat[i], beta);
    }
    y ~ normal(y_pred, sigma);
}
