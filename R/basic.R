lam_max <- function(A) {
  A <- (A + t(A)) / 2
  ev <- eigen(A, only.values = TRUE) # Note: maybe use irlba()
  max(ev$values)
}

inv <- function(A, tol1 = 1e-12, tol2 = 1e-8, lambda = NULL) {
  if (!is.matrix(A)) stop("inv() requires a matrix input.")

  cond_val <- rcond(A)

  if (cond_val > tol2) {
    G <- solve(A)
  } else if (cond_val > tol1) {
    warning(sprintf("Matrix is near-singular (rcond = %.3e), applying ridge regularization.", cond_val))
    if (is.null(lambda)) lambda <- 1e-4 * mean(diag(A))  # adaptive ridge
    A_stable <- A + diag(lambda, ncol(A))
    G <- solve(A_stable)
  } else {
    warning(sprintf("Matrix is numerically singular (rcond = %.3e), using generalized inverse.", cond_val))
    G <- MASS::ginv(A)
  }

  dimnames(G) <- dimnames(A)
  return(G)
}



get_weight <- function(X, beta, family = gaussian()) {
  if (is.function(family)) family <- family()

  if (identical(family$family, "gaussian")) {
    return(rep(1, nrow(X)))
  }

  # for more info., check `?stats::family`
  eta <- X %*% beta
  mu <- family$linkinv(eta)
  Vmu <- family$variance(mu)
  dmu_deta <- family$mu.eta(eta)
  (dmu_deta^2) / Vmu
}
