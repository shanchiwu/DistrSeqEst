// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
NumericVector a_opt_trace(const arma::mat& X_now,
                          const arma::mat& X_new,
                          const arma::vec& w_now,
                          const arma::vec& w_new) {
  // Convert inputs to Armadillo types
  const int n = X_new.n_rows;
  const int p = X_now.n_cols;

  // Fisher info matrix
  arma::mat W = diagmat(w_now);        // diag(w_i)
  arma::mat Oj = X_now.t() * W * X_now;
  Oj += 1e-6 * arma::eye(p, p);       // regularize
  arma::mat Oj_inv;
  try {
    Oj_inv = inv_sympd(Oj);
  } catch (...) {
    Oj_inv = arma::pinv(Oj); // generalized inverse fallback
  }

  double base_trace = arma::trace(Oj_inv); // Pre-compute trace(Oj_inv) once

  // Pre-allocate memory
  arma::vec traces(n);
  arma::vec x(p), v(p);

  for (int i = 0; i < n; ++i) {
    x = X_new.row(i).t();  // 1 x p => p x 1
    double w = w_new[i];

    v = Oj_inv * x;
    double trace_adj = w / (1.0 + w * arma::dot(x, v)) * arma::dot(v, v);

    traces[i] = base_trace - trace_adj;
  }

  return wrap(traces);
}
