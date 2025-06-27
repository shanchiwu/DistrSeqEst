#' @title Optimal Design Selection Criteria
#'
#' @description
#' Selects a data point from a set of unlabeled candidates based on specific optimal design criteria.
#' Currently supports **D-optimality** and **A-optimality** criteria for sequential or adaptive experimental design.
#'
#' @param method A character string specifying the selection method (e.g., \code{"D.opt"}, \code{"A.opt"}, or \code{"random"}). Can also be a user-defined function.
#' @param X A numeric matrix of covariates. Each row represents an observation and each column a variable.
#' @param w A numeric vector of weights corresponding to each observation in \code{X}.
#' @param labeled_id Integer vector specifying the indices of already selected (labeled) observations.
#' @param unlabeled_id Integer vector specifying the indices of candidate (unlabeled) observations to evaluate.
#' @param ... Additional arguments passed to the specific method function.
#'
#' @details
#' This function implements optimal design selection under weighted settings, where each observation is associated with a weight matrix \code{W}.
#'
#' \strong{D-optimality} seeks to maximize the determinant of the Fisher information matrix:
#' \deqn{\text{D-optimality: } \quad \max \det(\mathbf{X}'\mathbf{W}\mathbf{X})}
#'
#' \strong{A-optimality} minimizes the trace of the inverse of the information matrix:
#' \deqn{\text{A-optimality: } \quad \min \operatorname{tr}((\mathbf{X}'\mathbf{W}\mathbf{X})^{-1})}
#'
#' Let \eqn{\mathbf{X}_{\text{now}}} be the design matrix for currently labeled observations and \eqn{\mathbf{X}_{\text{new}}} be the matrix for candidate points.
#' The weight vector \eqn{\mathbf{w}} is partitioned accordingly into \eqn{\mathbf{w}_{\text{now}}} and \eqn{\mathbf{w}_{\text{new}}}.
#'
#' \strong{D-optimality}:
#' The method selects the point that maximizes the determinant of the weighted information matrix after adding the candidate.
#' Using **Sylvester's determinant identity**, the determinant ratio (i.e., the gain from adding \eqn{\mathbf{x}_i}) is computed as:
#' \deqn{\frac{\det(\mathbf{A} + w_i \mathbf{x}_i \mathbf{x}_i^\top)}{\det(\mathbf{A})} = 1 + w_i \cdot \mathbf{x}_i^\top \mathbf{A}^{-1} \mathbf{x}_i}
#' where \eqn{\mathbf{A} = \mathbf{X}_{\text{now}}^\top \mathbf{W}_{\text{now}} \mathbf{X}_{\text{now}} + \lambda \mathbf{I}} includes a ridge penalty for numerical stability.
#' The point maximizing this expression is selected exactly.
#'
#' \strong{A-optimality}:
#' The method selects the point that minimizes the trace of the inverse of the updated weighted information matrix.
#' Using the **Sherman–Morrison formula**, we compute the updated inverse:
#' \deqn{
#' \operatorname{tr}((\mathbf{A} + w_i \mathbf{x}_i \mathbf{x}_i^\top)^{-1}) =
#' \operatorname{tr}(\mathbf{A}^{-1}) - \frac{w_i \cdot \|\mathbf{A}^{-1} \mathbf{x}_i\|^2}{1 + w_i \cdot \mathbf{x}_i^\top \mathbf{A}^{-1} \mathbf{x}_i}
#' }
#' where again \eqn{\mathbf{A}} is the weighted information matrix.
#' The point with minimal updated trace is selected.
#'
#' These formulas avoid recomputing the full matrix inverse for each candidate and ensure exact evaluation under some assumptions.
#'
#' The A-optimal trace computation is accelerated using C++ via Rcpp.
#'
#'
#' @return An integer indicating the row index (relative to the input \code{X}) of the selected optimal data point.
#'
#'
#' @examples
#' \dontrun{
#'   X <- matrix(rnorm(100 * 5), 100, 5)
#'   w <- rep(1, 100)
#'   labeled <- sample(1:100, 10)
#'   unlabeled <- setdiff(1:100, labeled)
#'   design_select.D.opt(X, w, labeled, unlabeled)
#'   design_select.A.opt(X, w, labeled, unlabeled)
#' }
#'
#' @name optimal
#' @rdname optimal
#' @export
design_select.D.opt <- function(method, X, w, labeled_id, unlabeled_id, ...){
  X_now <- X[labeled_id, , drop = FALSE]
  X_new <- X[unlabeled_id, , drop = FALSE]

  w_now <- w[labeled_id]
  w_new <- w[unlabeled_id]

  # row-wise quadratic form, efficient computation of x'Ax for each row x, <Ax,x>
  # Note: Maybe Cholesky decomposition would be faster and stable?
  A = crossprod(X_now, w_now * X_now) + diag(1e-6, ncol(X_now)) # add regularization avoid singular or ill-condition
  A_inv = inv(A) # pxn * nxn * nxp
  A_x = X_new %*% A_inv # nxp * pxp
  quadform = rowSums(A_x * X_new) # nxp * nxp, Hadamard (element-wise) multiplication

  score = quadform * w_new

  return(unlabeled_id[which.max(score)])
}


#' @rdname optimal
#' @export
design_select.A.opt <- function(method, X, w, labeled_id, unlabeled_id, ...) {
  X_now <- X[labeled_id, , drop = FALSE]
  X_new <- X[unlabeled_id, , drop = FALSE]

  w_now <- w[labeled_id]
  w_new <- w[unlabeled_id]

  trs <- a_opt_trace(X_now, X_new, w_now, w_new) # implement in C++

  best_idx <- which.min(trs)
  return(unlabeled_id[best_idx])
}


#' @rdname optimal
#' @export
design_select.random <- function(method, X, w, labeled_id, unlabeled_id, ...) {
  sample(unlabeled_id, 1)
}


#' @rdname optimal
#' @export
design_select.default <- function(method, X, w, labeled_id, unlabeled_id, ...) {
  stop(sprintf("Unknown method '%s'. Please define method using S3 method: design_select.%s",
               class(method)[1], class(method)[1]))

}
