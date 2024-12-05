#include <Rcpp.h>
using namespace Rcpp;

//' @name BootLM
//' @export
//' @title Bootstrap LM
//' @description C++ implementation of the bootstrapping LM loop used in
//' \link[ensembleR]{fitAggregationFunction_bootLM}. This function is not
//' intended to be called directly.
//' @param x A numeric matrix of features
//' @param y A numeric vector of the response variable
//' @param boot_iter An integer specifying the number of bootstrap iterations.
// [[Rcpp::export]]
NumericMatrix bootLM( NumericMatrix x, NumericVector y, int boot_iter = 1000) {
  int nrow = x.nrow();
  int ncol = x.ncol();

  // Since these R functions are already optimized in C++, just invoke them
  Function chol("chol");
  Function forwardsolve("forwardsolve");
  Function backsolve("backsolve");
  Function matmul("%*%");

  // Create a matrix to store the bootstrapped beta values
  Rcpp::NumericMatrix beta_mat(ncol, boot_iter);

  // For each bootstrap, fit the linear model and store the beta coefficients
  // Relying on the speed on Rcpp for the looping over base R
  for (int i = 0; i < boot_iter; i++) {
      IntegerVector idx = Rcpp::sample(nrow, nrow, true) - 1;
      Rcpp::NumericVector y_boot = y[idx];
      Rcpp::NumericMatrix x_boot(nrow, ncol);
      for (int ix = 0; ix < nrow; ix++) {
          for (int j = 0; j < ncol; j++)
            x_boot(ix, j) = x(idx[ix], j);
      }
      // multiple transpose(x_boot) and x_boot with matrix multiplication
      NumericMatrix u = chol(matmul(transpose(x_boot), x_boot));
      NumericVector betas = backsolve(u, forwardsolve(transpose(u), matmul(transpose(x_boot), y_boot)));
      beta_mat(_, i) = betas;

  }
  return beta_mat;
}
