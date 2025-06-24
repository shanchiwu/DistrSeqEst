lam_max = function(A){
  A <- (A + t(A)) / 2
  ev = eigen(A, only.values = T)
  #ev = (irlba::irlba(A, nv = 1)$d)^2 # Note: maybe use irlba
  return(max(ev$values))
}

inv <- function(A, tol = 1e-12) {
  if (!is.matrix(A)) stop("inv() requires a matrix input.")

  if (rcond(A) < tol) {
    # message(sprintf("Warning: Matrix is ill-conditioned (rcond = %.3e), applying ridge regularization.\n", rcond(A)))
    # lambda <- 1e-4 * mean(diag(A))
    # A <- A + diag(lambda, ncol(A))
    warning(sprintf("Matrix is ill-conditioned (rcond = %.3e). Using MASS::ginv().\n", rcond(A)))
    G <- MASS::ginv(A)
    dimnames(G) <- dimnames(A)
    return(G)
  }

  G <- solve(A)
  dimnames(G) <- dimnames(A)
  return(G)
}



get_weight <- function(X, beta, family) {
  # for more info., check `?stats::family`
  eta <- X %*% beta
  mu <- family$linkinv(eta)
  Vmu <- family$variance(mu)
  dmu_deta <- family$mu.eta(eta)
  (dmu_deta^2) / Vmu
}
