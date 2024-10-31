data {
  int<lower=0> N;               // Number of data samples
  int<lower=0> K;               // Number of models
  vector[N] y_true;             // Vector of true labels
  matrix[N, K] f_pred;          // Prediction matrix for each model; each column represents predictions from one model
}
parameters {
  simplex[K] w;                 // Weights for each model; 'simplex' type ensures the weights sum to 1
  real<lower=0> sigma;          // Standard deviation of the prediction error
}
model {
  w ~ dirichlet(rep_vector(1.0, K));  // Dirichlet prior for weights, with all parameters set to 1 (indicating equal prior belief for each model)
  sigma ~ normal(0, 1);                // Prior for sigma, with a normal distribution centered at 0 and a standard deviation of 1
  y_true ~ normal(f_pred * w, sigma);  // Model the true labels as normally distributed around a weighted sum of predictions, with standard deviation sigma
}
