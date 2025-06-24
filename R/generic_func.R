#' @title Printing Sequential Estimation Result
#' @description
#' Print a sequential estimation result
#'
#' @param x an object of class \code{"seq.fit"}
#' @param ... optional arguments to print methods.
#'
#' @method print seq.fit
#' @export
print.seq.fit <- function(x, ...) {
  cat("Object of class <seq.fit>\n")
  cat("---------------------------\n")
  cat("Interest terms :", paste(x$interest_term, collapse = ", "), "\n")
  cat("Sample size    :", x$Nj, "\n")
  cat("Object size    :", format(utils::object.size(x), units = "auto"), "\n")
  cat("Use summary() to view model summary.\n")
  invisible(x)
}

#' @title Printing Distributed Sequential Estimation Result
#' @description
#' Print a distributed sequential estimation result
#'
#' @param x an object of class \code{"distr.seq.fit"}
#' @param ... optional arguments to print methods.
#'
#' @method print distr.seq.fit
#' @export
print.distr.seq.fit <- function(x, ...) {
  cat("Object of class <distr.seq.fit>\n")
  cat("=================================\n")
  cat("Number of sequences :", length(x$fits), "\n")
  cat("Total sample size   :", x$N_star, "\n")
  cat("Object size         :", format(utils::object.size(x), units = "auto"), "\n")
  cat("Use summary() to view aggregated results.\n")
  invisible(x)
}





#' @title Sequential Estimation Result Object
#'
#' @description
#' An object of class \code{"seq.fit"} returned by \code{\link{seq_ana}}.
#' It contains the results of a sequential estimation procedure, including coefficient paths, information matrix, stopping info, and AUC (if applicable).
#'
#' @param object an object of class \code{"seq.fit"}, usually, a result of a call to \code{\link{seq_ana}}.
#' @param ... additional arguments affecting the summary produced.
#'
#' @seealso \code{\link{seq_ana}}, \code{\link{summary}}
#'
#' @method summary seq.fit
#' @export
summary.seq.fit <- function(object, ...) {
  out <- list(
    interest_term = object$interest_term,
    Nj = object$Nj,
    coef_est = stats::coef(object$fit),
    mu = object$mu,
    Sigma = object$Sigma,
    auc = if (!is.null(object$auc_fit)) pROC::auc(object$auc_fit) else NULL,
    time = object$time
  )
  class(out) <- "summary.seq.fit"
  return(out)
}

#' @rdname summary.seq.fit
#' @param x an object of class \code{"summary.seq.fit"}
#'
#' @method print summary.seq.fit
#' @export
print.summary.seq.fit <- function(x, ...) {
  cat("Summary of Sequential Estimation\n")
  cat("----------------------------------\n")
  cat("Interest Terms :", paste(x$interest_term, collapse = ", "), "\n")
  cat("Sample Size    :", x$Nj, "\n\n")
  cat("Coefficients:\n")
  print(round(x$coef_est, 4))
  cat("\nVariance-Covariance Matrix:\n")
  print(round(solve(x$Sigma),4))
  cat("\nStopping (mu)  :", round(x$mu, 4), "\n")
  cat("Time Taken     :", format(x$time), "\n")
  if (!is.null(x$auc)) {
    cat("AUC            :", round(x$auc, 4), "\n")
  }
  invisible(x)
}



#' @title Distributed Sequential Estimation Result Object
#'
#' @description
#' An object of class \code{"distr.seq.fit"} returned by \code{\link{distr_seq_ana}}.
#' It summarizes federated results across multiple sequential procedures.
#'
#' @param object an object of class \code{"distr.seq.fit"}, usually, a result of a call to \code{\link{distr_seq_ana}}.
#' @param ... additional arguments affecting the summary produced.
#'
#' @seealso \code{\link{distr_seq_ana}}, \code{\link{summary}}
#'
#' @method summary distr.seq.fit
#' @export
summary.distr.seq.fit <- function(object, ...) {
  out <- list(
    M = length(object$fits),
    Nj = object$Nj,
    beta_est = object$beta_est,
    Sigma_est = object$Sigma_est,
    auc = if (!is.null(object$auc_fit)) pROC::auc(object$auc_fit) else NULL,
    time_total = object$time_total
  )
  class(out) <- "summary.distr.seq.fit"
  return(out)
}

#' @rdname summary.distr.seq.fit
#'
#' @param x an object of class \code{"summary.distr.seq.fit"}
#'
#' @method print summary.distr.seq.fit
#' @export
print.summary.distr.seq.fit <- function(x, ...) {
  cat("Summary of Distributed Sequential Estimation\n")
  cat("=============================================\n")
  cat("Number of sequences :", x$M, "\n")
  cat("Samples per seq     :", paste(x$Nj, collapse = ", "), "\n\n")
  cat("Coefficients:\n")
  print(round(x$beta_est, 4))
  cat("\nVariance-Covariance Matrix:\n")
  print(round(solve(x$Sigma_est),4))
  cat("\nTotal Time          :", format(x$time_total), "\n")
  if (!is.null(x$auc)) {
    cat("Overall AUC         :", round(x$auc, 4), "\n")
  }
  invisible(x)
}
