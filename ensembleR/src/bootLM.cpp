#include <Rcpp.h>
using namespace Rcpp;

//' @export
//' @describeIn aggregationFunction_bootLM Run bootstrapped linear regression in Rcpp.
// [[Rcpp::export]]
NumericMatrix bootLM( NumericMatrix x, NumericVector y, int boot_iter = 100) {
  int nrow = x.nrow();
  int ncol = x.ncol();
  Function chol("chol");
  Function forwardsolve("forwardsolve");
  Function backsolve("backsolve");
  Function matmul("%*%");

  Rcpp::NumericMatrix beta_mat(ncol, boot_iter);
  // Rcpp::NumericMatrix y_mat(nrow, boot_iter);
  for (int i = 0; i < boot_iter; i++) {
      IntegerVector idx = Rcpp::sample(nrow, nrow, true) - 1;
      // Rcpp::NumericMatrix x_boot = x[idx, _];
      Rcpp::NumericVector y_boot = y[idx];
      // y_mat(_, i) = y_boot;
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
