#' @importFrom graphics abline matplot
#' @title Plot coefficient paths from sequential estimation
#'
#' @description
#' This function plots the estimated coefficient path of a specified variable across
#' multiple sequential fits. It handles coefficient paths of unequal lengths by padding
#' them with \code{NA}s.
#'
#' @param x A fitted object class \code{distr.seq.fit}.
#' @param var A character string specifying the variable name in the coefficient path
#'            to be plotted (must be a column name in \code{coef_path}, see
#'            \code{\link{distr_seq_ana}}).
#'
#' @details
#' The function collects the coefficient trajectories of the selected variable from each
#' sequential fit. Since the trajectories may vary in length, the function pads them with
#' \code{NA}s to align their lengths and uses \code{matplot()} to plot them on the same graph.
#' A horizontal dashed line is added to indicate the final estimated coefficient value.
#'
#' @return A plot is generated as a side effect. The function does not return a value.
#'
#' @examples
#' \dontrun{
#' plot_coef(fit_all, "X1")
#' }
#'
#' @export

plot_coef <- function(x, var) {
  if (!var %in% names(x$beta_est)) {
    stop("The specified `var` is not found among the names of `x$beta_est`.")
  }

  M <- length(x$fits)
  coef_list <- lapply(x$fits[1:M], function(fit, var) fit$coef_path[, var], var = var)

  max_len <- max(sapply(coef_list, length))

  coef_mat <- sapply(coef_list, function(x) {
    length(x) <- max_len
    x
  })

  matplot(coef_mat, type = "l", lty = 1, col = 1:5,
          xlab = "Index", ylab = paste(var, "coefficient"),
          main = paste("Coefficient paths for", var, "( Sequence 1 -", M, ")"))

  abline(h = x$beta_est[var], col = "gray", lty = 2)
}
